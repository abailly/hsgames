use std::fmt::{self, Display, Formatter};

use crate::{
    robot::possible_hits, Event, GameEngine, Input, Nation, Offensive, Output, Phase, Player, Side,
    TechnologyType,
};

pub struct Robot {
    side: Side,
    depth: u8,
    next_move: Move,
}

impl Robot {
    pub fn new(side: Side, depth: u8) -> Self {
        Robot {
            side,
            depth,
            next_move: Move::Pass,
        }
    }
}

impl Player for Robot {
    fn output(&mut self, message: &Output, engine: &GameEngine) {
        match message {
            Output::ChooseInitiative => {
                let best_move = best_move(self.side, engine, self.depth, vec![], vec![]);
                if let Some(m) = best_move {
                    self.next_move = m;
                }
            }
            Output::ImproveTechnologies(available) => {
                let best_move =
                    best_move(self.side, engine, self.depth, vec![], available.to_vec());
                if let Some(m) = best_move {
                    self.next_move = m;
                }
            }
            Output::LaunchOffensive(available) => {
                let mut played = engine.all_nations_at_war(self.side);
                played.retain(|n| !available.contains(n));
                let best_move = best_move(self.side, engine, self.depth, played, vec![]);
                if let Some(m) = best_move {
                    self.next_move = m;
                }
            }
            Output::ReinforceNations => {
                let best_move = best_move(self.side, engine, self.depth, vec![], vec![]);
                if let Some(m) = best_move {
                    self.next_move = m;
                }
            }
            Output::SelectNationForHit => {
                let nations = possible_hits(&self.side, &engine.state);
                self.next_move = Move::Hit(nations[0].0);
            }
            other => {
                println!("Robot received message: {:?}", other);
            }
        }
    }

    fn input(&mut self) -> Input {
        match self.next_move {
            Move::BetForInitiative(_, pr) => Input::Number(pr as u8),
            Move::EventsDrawn(_) => panic!("Cannot input events"),
            Move::ResourcesCollected => panic!("Cannot input resources"),
            Move::ImproveTechnology(_, tech, pr) => Input::Select(tech.category, pr),
            Move::Pass => Input::Pass,
            Move::NextTurn => panic!("Cannot input NextTurn"),
            Move::Offensive(from, to, pr) => Input::Offensive(from, to, pr),
            Move::Reinforce(nation, pr) => Input::Reinforce(nation, pr),
            Move::Hit(nation) => Input::ApplyHit(nation),
        }
    }

    fn out(&self) -> Vec<Output> {
        vec![]
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Move {
    BetForInitiative(Side, u8),
    EventsDrawn(Vec<Event>),
    ResourcesCollected,
    ImproveTechnology(Side, crate::Technology, u8),
    Pass,
    NextTurn,
    Offensive(Nation, Nation, u8),
    Reinforce(Nation, u8),
    Hit(Nation),
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
            Move::Reinforce(nation, pr) => write!(f, "Reinforce({:?}, {})", nation, pr),
            Move::Hit(nation) => write!(f, "Hit({:?})", nation),
        }
    }
}

#[derive(Debug, Clone)]
struct Search {
    /// The side that is maximizing
    me: Side,

    /// The state of the game
    engine: GameEngine,

    /// The bet of the allies for the initiative
    /// TODO: This should be part of the state
    allies_initiative: u8,

    /// TODO: This should be part of the state
    nations_played: Vec<Nation>,

    /// The moves that have been played to reach this state
    moved: Option<Move>,

    /// TODO: This should be part of the state
    available_tech: Vec<TechnologyType>,
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
                let resources = self.engine.state.resources_for(&side);
                let mut available_techs = self.engine.state.available_technologies(&side);
                available_techs.retain(|t| self.available_tech.contains(&t.category));

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
                sources.retain(|n| !self.nations_played.contains(n));

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
            Phase::Reinforcements(side) => {
                let mut moves = vec![];
                let resources = self.engine.state.resources_for(&side);
                let nations = self
                    .engine
                    .all_nations_at_war(side)
                    .iter()
                    .filter_map(|n| {
                        let losses = n.maximum_breakdown() - self.engine.state.breakdown_level(n);
                        if losses > 0 && !self.nations_played.contains(n) {
                            Some((*n, losses))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<(Nation, u8)>>();

                for (nation, losses) in nations.iter() {
                    for pr in 1..=*losses {
                        if pr <= resources {
                            moves.push(Move::Reinforce(*nation, pr));
                        }
                    }
                }
                moves.push(Move::Pass);
                SearchIterator {
                    search: self,
                    moves,
                }
            }
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
                self.available_tech.retain(|t| t != &tech.category);
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
                let result = self.engine.resolve_offensive(&offensive);
                self.nations_played.push(*from);
                self.engine
                    .set_phase(Phase::LaunchOffensives(self.engine.state.initiative));
            }
            Move::Reinforce(nation, pr) => {
                self.nations_played.push(*nation);
                self.engine.reinforce(*nation, *pr);
                self.engine
                    .set_phase(Phase::Reinforcements(self.engine.state.initiative));
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
                    self.nations_played = vec![];
                    self.engine.set_phase(Phase::LaunchOffensives(side));
                }
                Phase::LaunchOffensives(side) => {
                    self.nations_played = vec![];
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
                        self.engine.set_phase(Phase::Blockade);
                    }
                }
                Phase::Blockade => {
                    if self.engine.state.initiative == Side::Empires {
                        self.engine.set_phase(Phase::NewTurn);
                    } else {
                        self.engine.set_phase(Phase::UBoot);
                    }
                }
                Phase::NewTurn => panic!("Cannot pass in NewTurn phase"),
            },
            Move::NextTurn => {
                self.engine.new_turn();
            }
            Move::Hit(_) => panic!("Cannot apply Hit move"),
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

fn alphabeta(
    search: &mut Search,
    maximizing: bool,
    depth: u8,
    mut alpha: f64,
    mut beta: f64,
) -> f64 {
    if depth <= 0 || search.game_ends() {
        return search.valuation();
    }

    if maximizing {
        let mut value: f64 = -1.0;
        for mut child in search.iter() {
            let max = match child.engine.state.side_to_play() {
                Some(Side::Allies) => true,
                Some(Side::Empires) => false,
                None => child.me == Side::Allies,
            };

            value = value.max(alphabeta(&mut child, max, depth - 1, alpha, beta));
            alpha = alpha.max(value);
            if value >= beta {
                break;
            };
        }
        value
    } else {
        let mut value: f64 = 1.0;
        for mut child in search.iter() {
            let max = match child.engine.state.side_to_play() {
                Some(Side::Allies) => true,
                Some(Side::Empires) => false,
                None => child.me == Side::Allies,
            };
            value = value.min(alphabeta(&mut child, max, depth - 1, alpha, beta));
            beta = beta.min(value);
            if value <= alpha {
                break;
            }
        }
        value
    }
}

pub fn best_move(
    me: Side,
    engine: &GameEngine,
    depth: u8,
    nations_played: Vec<Nation>,
    available_tech: Vec<TechnologyType>,
) -> Option<Move> {
    let side = engine.state.side_to_play();
    let mut search = Search {
        me,
        engine: engine.clone(),
        allies_initiative: 0,
        nations_played,
        moved: None,
        available_tech,
    };
    match side {
        Some(s) if s == me => {
            println!("({:?}) searching for side: {:?} ", engine.state.phase, me);
            let mut best_value = if s == Side::Allies { -1.0 } else { 1.0 };
            let mut best_move = None;
            for mut child in search.iter() {
                let max = match child.engine.state.side_to_play() {
                    Some(Side::Allies) => true,
                    Some(Side::Empires) => false,
                    None => child.me == Side::Allies,
                };
                let value = alphabeta(&mut child, max, depth, -1.0, 1.0);
                println!(" {:?} = {}", child.moved, value);
                if s == Side::Allies && value > best_value {
                    best_value = value;
                    best_move = child.moved;
                } else if s == Side::Empires && value < best_value {
                    best_value = value;
                    best_move = child.moved;
                }
            }
            println!(" => best move: {:?}", best_move);
            best_move
        }
        _ => None,
    }
}

#[cfg(test)]
mod minimax_test {
    use crate::fixtures::EngineBuilder;

    use super::*;

    #[test]
    fn returns_engine_valuation_at_depth_0() {
        let engine = GameEngine::new(42);
        let mut search = Search {
            me: Side::Allies,
            engine: engine.clone(),
            allies_initiative: 0,
            nations_played: vec![],
            available_tech: vec![],
            moved: None,
        };
        let value = alphabeta(&mut search, true, 0, -1.0, 1.0);
        assert!((value - engine.valuation()).abs() < f64::EPSILON);
    }

    #[test]
    fn returns_state_valuation_at_depth_10() {
        let engine = GameEngine::new(42);
        let mut search = Search {
            me: Side::Allies,
            engine: engine.clone(),
            allies_initiative: 0,
            nations_played: vec![],
            available_tech: vec![],
            moved: None,
        };
        let value = alphabeta(&mut search, true, 10, -1.0, 1.0);
        assert!(value != engine.valuation());
    }

    #[test]
    fn returns_no_move_given_not_side_phase() {
        let engine = EngineBuilder::new(42).build();
        let best_move = best_move(Side::Allies, &engine, 10, vec![], vec![]);
        assert!(best_move.is_none());
    }

    #[test]
    fn returns_some_move_for_allies_given_its_side_phase_to_play() {
        let engine = EngineBuilder::new(42)
            .on_turn(2)
            .at_phase(Phase::LaunchOffensives(Side::Allies))
            .with_resources(Side::Allies, 10)
            .with_resources(Side::Empires, 10)
            .build();
        let best_move = best_move(Side::Allies, &engine, 6, vec![], vec![]);
        assert!(best_move.is_some());
    }

    #[test]
    fn returns_some_move_for_empires_given_its_empires_phase_to_play() {
        let engine = EngineBuilder::new(42)
            .on_turn(2)
            .at_phase(Phase::LaunchOffensives(Side::Empires))
            .with_resources(Side::Allies, 10)
            .with_resources(Side::Empires, 10)
            .build();
        let best_move = best_move(Side::Empires, &engine, 6, vec![], vec![]);
        assert!(best_move.is_some());
    }

    #[test]
    fn does_not_select_pass_as_best_move_on_turn_1() {
        let engine = EngineBuilder::new(42)
            .at_phase(Phase::ImproveTechnologies(Side::Allies))
            .with_resources(Side::Allies, 10)
            .with_resources(Side::Empires, 10)
            .build();
        let best_move = best_move(Side::Allies, &engine, 8, vec![], vec![]);
        assert!(best_move.unwrap() != Move::Pass);
    }

    #[test]
    fn robot_selects_best_move_when_it_plays_initiative() {
        let engine = EngineBuilder::new(14)
            .on_turn(2)
            .at_phase(Phase::Initiative(Side::Empires))
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = Robot::new(Side::Empires, 6);

        robot.output(&Output::ChooseInitiative, &engine);

        let input = robot.input();

        assert_eq!(Input::Number(2), input);
    }
}
