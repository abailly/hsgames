use std::{
    f64::{MAX, MIN},
    fmt::{self, Display, Formatter},
};

use crate::{Event, GameEngine, GameState, Nation, Offensive, Phase, Side};

#[derive(Debug, Clone)]
enum Move {
    BetForInitiative(Side, u8),
    EventsDrawn(Vec<Event>),
    ResourcesCollected,
    ImproveTechnology(Side, crate::Technology, u8),
    Pass,
    NextTurn,
    Offensive(Nation, Nation, u8),
}

impl Display for Move {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Move::BetForInitiative(side, bet) => write!(f, "BetForInitiative({:?}, {})", side, bet),
            Move::EventsDrawn(events) => write!(f, "EventsDrawn({:?})", events),
            Move::ResourcesCollected => write!(f, "ResourcesCollected"),
            Move::ImproveTechnology(side, tech, pr) => {
                write!(f, "ImproveTechnology({:?}, {:?}, {})", side, tech, pr)
            }
            Move::Pass => write!(f, "Pass"),
            Move::NextTurn => write!(f, "NextTurn"),
            Move::Offensive(from, to, pr) => write!(f, "Offensive({:?}, {:?}, {})", from, to, pr),
        }
    }
}

#[derive(Debug, Clone)]
struct Search {
    /// The side that is maximizing
    me: Side,

    /// The current side we are exploring in this node This is `me` if
    /// we are maximizing, and the opponent otherwise. We consider
    /// that we are maximizing for phases which are common to both
    /// sides.
    side: Side,

    /// The state of the game
    engine: GameEngine,

    /// The bet of the allies for the initiative
    /// TODO: This should be part of the state
    allies_initiative: u8,

    /// TODO: This should be part of the state
    offensive_sources: Vec<Nation>,

    /// The moves that have been played to reach this state
    moved: Option<Move>,
}

struct SearchIterator<'a> {
    search: &'a Search,
    moves: Vec<Move>,
}

impl Search {
    fn game_ends(&self) -> bool {
        self.engine.game_ends()
    }

    fn valuation(&self) -> f64 {
        self.engine.valuation()
    }

    /// Returns an iterator over "all" the possible moves from the current engine
    pub(crate) fn iter(&mut self) -> SearchIterator {
        match self.engine.state.phase {
            Phase::Initiative(Side::Allies) => {
                let mut moves = vec![];
                let resources = self.engine.state.resources_for(&Side::Allies);
                for i in 0..resources.min(3) {
                    moves.push(Move::BetForInitiative(Side::Allies, i));
                }
                SearchIterator {
                    search: self,
                    moves,
                }
            }
            Phase::Initiative(Side::Empires) => {
                let mut moves = vec![];
                let resources = self.engine.state.resources_for(&Side::Empires);
                for i in 0..resources.min(3) {
                    moves.push(Move::BetForInitiative(Side::Empires, i));
                }
                SearchIterator {
                    search: self,
                    moves,
                }
            }
            Phase::DrawEvents => {
                let mut moves = vec![];
                moves.push(Move::EventsDrawn(self.engine.draw_events()));
                SearchIterator {
                    search: self,
                    moves,
                }
            }
            Phase::CollectResources => {
                let mut moves = vec![];
                moves.push(Move::ResourcesCollected);
                SearchIterator {
                    search: self,
                    moves,
                }
            }
            Phase::ImproveTechnologies(side) => {
                let mut moves = vec![];
                let mut resources = self.engine.state.resources_for(&side);
                let mut available_techs = self.engine.state.available_technologies(&side);
                if resources > 0 && !available_techs.is_empty() {
                    let tech = available_techs.pop().unwrap();
                    let max_pr = resources.min(5);
                    for pr in 1..=max_pr {
                        moves.push(Move::ImproveTechnology(side, tech, pr));
                    }
                }
                moves.push(Move::Pass);
                SearchIterator {
                    search: self,
                    moves,
                }
            }
            Phase::LaunchOffensives(side) => {
                let mut moves = vec![];
                let resources = self.engine.state.resources_for(&side);
                let mut sources: Vec<Nation> = self.engine.all_nations_at_war(side);
                sources.retain(|n| !self.offensive_sources.contains(n));

                for source in sources.iter() {
                    let mut targets: Vec<&Nation> = self.engine.state.neighbours(source);
                    targets.sort();
                    for target in targets.iter() {
                        for pr in 1..=self.engine.state.operational_level(source) {
                            if pr <= resources {
                                moves.push(Move::Offensive(*source, **target, pr))
                            };
                        }
                    }
                }
                moves.push(Move::Pass);
                SearchIterator {
                    search: self,
                    moves,
                }
            }
            Phase::Reinforcements(_) => SearchIterator {
                search: self,
                moves: vec![Move::Pass],
            },
            Phase::UBoot => SearchIterator {
                search: self,
                moves: vec![Move::Pass],
            },
            Phase::Blockade => SearchIterator {
                search: self,
                moves: vec![Move::Pass],
            },
            Phase::NewTurn => SearchIterator {
                search: self,
                moves: vec![Move::NextTurn],
            },
        }
    }

    fn apply_move(&mut self, m: &Move) {
        match m {
            Move::BetForInitiative(Side::Allies, bet) => {
                self.allies_initiative = *bet;
                self.engine.set_phase(Phase::Initiative(Side::Empires));
            }
            Move::BetForInitiative(Side::Empires, bet) => {
                self.engine
                    .determine_initiative(self.allies_initiative, *bet);
                self.engine.set_phase(Phase::DrawEvents);
            }
            Move::EventsDrawn(events) => {
                for e in events {
                    self.engine.play_events(e);
                }
                self.engine.set_phase(Phase::CollectResources);
            }
            Move::ResourcesCollected => {
                self.engine.collect_resources();
                self.engine
                    .set_phase(Phase::ImproveTechnologies(self.engine.state.initiative));
            }
            Move::ImproveTechnology(side, tech, pr) => {
                self.engine
                    .try_improve_technology(*side, tech.category, *pr);
                self.engine
                    .set_phase(Phase::ImproveTechnologies(self.engine.state.initiative));
            }
            Move::Offensive(from, to, pr) => {
                let offensive = Offensive {
                    initiative: self.engine.state.initiative,
                    from: *from,
                    to: *to,
                    pr: *pr,
                };
                self.engine.resolve_offensive(&offensive);
                self.offensive_sources.push(*from);
                self.engine
                    .set_phase(Phase::LaunchOffensives(self.engine.state.initiative));
            }
            Move::Pass => match self.engine.state.phase {
                Phase::Initiative(Side::Allies) => {
                    self.engine.set_phase(Phase::Initiative(Side::Empires));
                }
                Phase::Initiative(Side::Empires) => {
                    self.engine.set_phase(Phase::DrawEvents);
                }
                Phase::DrawEvents => {
                    self.engine.set_phase(Phase::CollectResources);
                }
                Phase::CollectResources => {
                    self.engine
                        .set_phase(Phase::ImproveTechnologies(self.engine.state.initiative));
                }
                Phase::ImproveTechnologies(side) => {
                    self.offensive_sources = vec![];
                    self.engine.set_phase(Phase::LaunchOffensives(side));
                }
                Phase::LaunchOffensives(side) => {
                    self.engine.set_phase(Phase::Reinforcements(side));
                }
                Phase::Reinforcements(side) => match side {
                    Side::Allies => self.engine.set_phase(Phase::Blockade),
                    Side::Empires => self.engine.set_phase(Phase::UBoot),
                },
                Phase::UBoot => {
                    if self.engine.state.initiative == Side::Allies {
                        self.engine.set_phase(Phase::NewTurn);
                    } else {
                        self.engine
                            .set_phase(Phase::ImproveTechnologies(Side::Allies));
                    }
                }
                Phase::Blockade => {
                    if self.engine.state.initiative == Side::Empires {
                        self.engine.set_phase(Phase::NewTurn);
                    } else {
                        self.engine
                            .set_phase(Phase::ImproveTechnologies(Side::Empires));
                    }
                }
                Phase::NewTurn => panic!("Cannot pass in NewTurn phase"),
            },
            Move::NextTurn => {
                self.engine.new_turn();
            }
        }
    }
}

impl Iterator for SearchIterator<'_> {
    type Item = Search;

    fn next(&mut self) -> Option<Search> {
        match &self.moves[..] {
            [] => None,
            [m, ..] => {
                let mut new_search = self.search.clone();
                new_search.apply_move(m);
                let moved = self.moves.remove(0);
                new_search.moved = Some(moved);
                Some(new_search)
            }
        }
    }
}

fn alphabeta(search: &mut Search, depth: u32, mut alpha: f64, mut beta: f64) -> f64 {
    println!(
        "depth: {}, phase: {:?}, turn: {}, alpha: {}, beta: {}, moved: {:?}",
        depth,
        search.engine.state.phase,
        search.engine.state.current_turn,
        alpha,
        beta,
        search.moved
    );
    if depth <= 0 || search.game_ends() {
        return search.valuation();
    }

    let maximizing = search
        .engine
        .state
        .side_to_play()
        .map_or(true, |s| s == search.me);

    if maximizing {
        let mut value: f64 = -1.0;
        for mut child in search.iter() {
            value = value.max(alphabeta(&mut child, depth - 1, alpha, beta));
            alpha = alpha.max(value);
            if value >= beta {
                break;
            };
        }
        value
    } else {
        let mut value: f64 = 1.0;
        for mut child in search.iter() {
            value = value.min(alphabeta(&mut child, depth - 1, alpha, beta));
            beta = beta.min(value);
            if value <= alpha {
                break;
            }
        }
        value
    }
}

#[cfg(test)]
mod minimax_test {
    use super::*;

    #[test]
    fn returns_engine_valuation_at_depth_0() {
        let engine = GameEngine::new(42);
        let mut search = Search {
            me: Side::Allies,
            side: Side::Allies,
            engine: engine.clone(),
            allies_initiative: 0,
            offensive_sources: vec![],
            moved: None,
        };
        let value = alphabeta(&mut search, 0, -1.0, 1.0);
        assert!((value - engine.valuation()).abs() < f64::EPSILON);
    }

    #[test]
    fn returns_state_valuation_at_depth_50() {
        let engine = GameEngine::new(42);
        let mut search = Search {
            me: Side::Allies,
            side: Side::Allies,
            engine: engine.clone(),
            allies_initiative: 0,
            offensive_sources: vec![],
            moved: None,
        };
        let value = alphabeta(&mut search, 10, -1.0, 1.0);
        println!("valuation: {}", value);
        assert!(value < engine.valuation());
    }
}
