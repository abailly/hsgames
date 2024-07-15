use core::fmt;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::side::*;
use crate::tech::*;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct WarState {
    pub resources: u8,
    pub vp: u8,
    pub technologies: Box<Technologies>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct GameState {
    pub current_turn: u8,
    pub initiative: Side,
    pub winner: Option<Side>,
    pub russian_revolution: u8,
    pub nations: HashMap<Nation, NationState>,
    pub countries: HashMap<Nation, Country>,
    pub state_of_war: HashMap<Side, WarState>,
    seed: u64,
    rng: StdRng,
    events_pool: Vec<Event>,
    active_events: Vec<ActiveEvent>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum HitsResult {
    Surrenders(Nation),
    Winner(Side),
    Hits(Nation, u8),
    NationNotAtWar(Nation),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Event {
    pub event_id: u8,
    pub year: u16,
    pub not_after: Option<u16>,
    pub title: &'static str,
}

impl Event {
    fn activate(&self) -> ActiveEvent {
        ActiveEvent {
            event: self.clone(),
            deactivation: |_game| true, // by default, events last for one turn
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ActiveEvent {
    pub event: Event,
    pub deactivation: fn(&GameState) -> bool,
}

pub const ALL_EVENTS: [Event; 13] = [
    Event {
        event_id: 1,
        year: 1914,
        not_after: None,
        title: "All is quiet",
    },
    Event {
        event_id: 2,
        year: 1914,
        not_after: None,
        title: "All is quiet",
    },
    Event {
        event_id: 3,
        year: 1914,
        not_after: Some(1914),
        title: "Schlieffen plan",
    },
    Event {
        event_id: 4,
        year: 1914,
        not_after: Some(1914),
        title: "Race to the sea",
    },
    Event {
        event_id: 5,
        year: 1915,
        not_after: None,
        title: "Shells crisis",
    },
    Event {
        event_id: 6,
        year: 1915,
        not_after: None,
        title: "Gas!",
    },
    Event {
        event_id: 7,
        year: 1915,
        not_after: None,
        title: "Von Lettow in Africa",
    },
    Event {
        event_id: 8,
        year: 1915,
        not_after: None,
        title: "Gallipoli",
    },
    Event {
        event_id: 9,
        year: 1915,
        not_after: None,
        title: "Towards separated peace?",
    },
    Event {
        event_id: 10,
        year: 1915,
        not_after: None,
        title: "Italy enters the war!",
    },
    Event {
        event_id: 11,
        year: 1915,
        not_after: None,
        title: "Bulgaria enters the war!",
    },
    Event {
        event_id: 12,
        year: 1915,
        not_after: None,
        title: "Lusitiania sank",
    },
    Event {
        event_id: 13,
        year: 1915,
        not_after: None,
        title: "All is quiet",
    },
];

impl Display for HitsResult {
    #[allow(unused_must_use)]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            HitsResult::Surrenders(nation) => writeln!(f, "{} surrenders", nation),
            HitsResult::Winner(side) => writeln!(f, "{} wins", side),
            HitsResult::Hits(nation, hits) => writeln!(f, "{} takes {} hits", nation, hits),
            HitsResult::NationNotAtWar(nation) => writeln!(f, "{} is not at war", nation),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct GameLogic {
    pub(crate) previous: Option<Box<GameLogic>>,
    pub(crate) compute_bonus: fn(&GameState, &Offensive) -> (u8, u8, u8),
    pub(crate) roll_offensive_dice: fn(&mut GameState, u8) -> Vec<u8>,
    pub(crate) roll_artillery_dice: fn(&mut GameState, u8) -> Vec<u8>,
    pub(crate) evaluate_attack_hits: fn(&GameState, u8, u8, &Offensive, Vec<u8>) -> u8,
    pub(crate) evaluate_artillery_hits: fn(&GameState, &Offensive, Vec<u8>) -> u8,
    pub(crate) reduce_pr: fn(&mut GameState, Side, u8) -> &mut GameState,
    pub(crate) apply_hits: fn(&mut GameState, &Nation, u8) -> HitsResult,
}

fn default_reduce_pr(state: &mut GameState, side: Side, pr: u8) -> &mut GameState {
    let st = state.state_of_war.get_mut(&side).unwrap();
    if st.resources >= pr {
        st.resources -= pr;
    }
    state
}

fn default_compute_bonus(state: &GameState, offensive: &Offensive) -> (u8, u8, u8) {
    let max_attacker_tech_level = state.countries.get(&offensive.from).unwrap().max_tech_level;
    let max_defender_tech_level = state.countries.get(&offensive.to).unwrap().max_tech_level;

    let artillery_bonus = state
        .artillery_bonus(&offensive.initiative)
        .min(max_attacker_tech_level);
    let attack_bonus = state.adjust_attack_bonus(
        state
            .attack_bonus(&offensive.initiative)
            .min(max_attacker_tech_level),
        &offensive.from,
        &offensive.to,
    );
    let defense_malus = state
        .defense_bonus(&offensive.initiative.other())
        .min(max_defender_tech_level);
    (artillery_bonus, attack_bonus, defense_malus)
}

fn default_evaluate_artillery_hits(
    state: &GameState,
    offensive: &Offensive,
    artillery_dice: Vec<u8>,
) -> u8 {
    let bomb_country =
        |die: u8| return die >= state.countries.get(&offensive.from).unwrap().attack_factor;
    let artillery_hits = artillery_dice
        .iter()
        .map(|die| bomb_country(*die))
        .filter(|hit| *hit)
        .count() as u8;
    artillery_hits
}

fn default_evaluate_attack_hits(
    state: &GameState,
    attack_bonus: u8,
    defense_malus: u8,
    offensive: &Offensive,
    dice: Vec<u8>,
) -> u8 {
    let attack_country = |die: u8| {
        return die + attack_bonus - defense_malus
            >= state.countries.get(&offensive.from).unwrap().attack_factor;
    };
    let attack_hits = dice
        .iter()
        .map(|die| attack_country(*die))
        .filter(|hit| *hit)
        .count() as u8;
    attack_hits
}

fn default_roll_artillery_dice(state: &mut GameState, artillery_bonus: u8) -> Vec<u8> {
    (0..artillery_bonus).map(|_| state.roll()).collect()
}

fn default_roll_offensive_dice(state: &mut GameState, pr: u8) -> Vec<u8> {
    (0..pr).map(|_| state.roll()).collect()
}

fn default_apply_hits(state: &mut GameState, to: &Nation, hits: u8) -> HitsResult {
    if let NationState::AtWar(breakdown) = state.nations.get_mut(to).unwrap() {
        if hits >= *breakdown {
            state.surrenders(to)
        } else {
            *breakdown -= hits;
            HitsResult::Hits(*to, hits)
        }
    } else {
        HitsResult::NationNotAtWar(*to)
    }
}

impl GameLogic {
    fn new() -> Self {
        GameLogic {
            previous: None,
            compute_bonus: default_compute_bonus,
            roll_offensive_dice: default_roll_offensive_dice,
            roll_artillery_dice: default_roll_artillery_dice,
            evaluate_attack_hits: default_evaluate_attack_hits,
            evaluate_artillery_hits: default_evaluate_artillery_hits,
            reduce_pr: default_reduce_pr,
            apply_hits: default_apply_hits,
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Offensive {
    pub initiative: Side,
    pub from: Nation,
    pub to: Nation,
    pub pr: u8,
}

pub struct GameEngine {
    pub(crate) state: Box<GameState>,
    logic: Box<GameLogic>,
}

impl GameEngine {
    pub fn new(seed: u64) -> Self {
        GameEngine {
            state: Box::new(GameState::new(seed)),
            logic: Box::new(GameLogic::new()),
        }
    }

    pub fn with_state(state: Box<GameState>) -> GameEngine {
        GameEngine {
            state,
            logic: Box::new(GameLogic::new()),
        }
    }

    pub fn reduce_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        (self.logic.reduce_pr)(&mut self.state, side, pr);
        self
    }

    pub fn increase_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        self.state.increase_pr(side, pr);
        self
    }

    pub fn roll(&mut self) -> u8 {
        self.state.roll()
    }

    pub fn current_year(&self) -> u16 {
        self.state.current_year()
    }

    pub fn all_nations_at_war(&self, initiative: Side) -> Vec<Nation> {
        self.state.all_nations_at_war(initiative)
    }

    pub(crate) fn reinforce(&mut self, nation: Nation, pr: u8) -> &Self {
        let nation_state = self.state.nations.get_mut(&nation).unwrap();
        let maximum_breakdown = nation.maximum_breakdown();
        let current_breakdown = nation_state.breakdown();

        let (spent, reinforcement) =
            (1..=(pr + 1)).fold((0, 0), |(spent, reinforcement), resource| {
                if spent + resource <= pr && reinforcement + current_breakdown < maximum_breakdown {
                    (spent + resource, reinforcement + 1)
                } else {
                    (spent, reinforcement)
                }
            });
        nation_state.reinforce(reinforcement);
        self.reduce_pr(nation.side(), spent);
        self
    }

    pub(crate) fn apply_hits(&mut self, to: &Nation, hits: u8) -> HitsResult {
        (self.logic.apply_hits)(&mut self.state, to, hits)
    }

    pub(crate) fn draw_events(&mut self) -> Vec<Event> {
        self.state.draw_events()
    }

    pub(crate) fn resolve_offensive(&mut self, offensive: &Offensive) -> HitsResult {
        let (artillery_bonus, attack_bonus, defense_malus) = self.compute_bonus(offensive);

        let dice: Vec<u8> = self.roll_offensive_dice(offensive.pr);
        let artillery_dice: Vec<u8> = self.roll_artillery_dice(artillery_bonus);

        let attack_hits = self.evaluate_attack_hits(attack_bonus, defense_malus, offensive, dice);

        let artillery_hits = self.evaluate_artillery_hits(offensive, artillery_dice);

        self.reduce_pr(offensive.initiative, offensive.pr);
        self.apply_hits(&offensive.to, attack_hits + artillery_hits)
    }

    fn evaluate_artillery_hits(&mut self, offensive: &Offensive, artillery_dice: Vec<u8>) -> u8 {
        (self.logic.evaluate_artillery_hits)(&mut self.state, offensive, artillery_dice)
    }

    fn evaluate_attack_hits(
        &mut self,
        attack_bonus: u8,
        defense_malus: u8,
        offensive: &Offensive,
        dice: Vec<u8>,
    ) -> u8 {
        (self.logic.evaluate_attack_hits)(
            &mut self.state,
            attack_bonus,
            defense_malus,
            offensive,
            dice,
        )
    }

    fn roll_artillery_dice(&mut self, artillery_bonus: u8) -> Vec<u8> {
        (self.logic.roll_artillery_dice)(&mut self.state, artillery_bonus)
    }

    fn roll_offensive_dice(&mut self, pr: u8) -> Vec<u8> {
        (self.logic.roll_offensive_dice)(&mut self.state, pr)
    }

    fn compute_bonus(&self, offensive: &Offensive) -> (u8, u8, u8) {
        (self.logic.compute_bonus)(&self.state, offensive)
    }

    pub(crate) fn new_turn(&mut self) -> &Self {
        self.state.new_turn();
        self
    }

    pub(crate) fn activate_event(&mut self, event: &Event) {
        self.state.activate_event(event);
    }
}

impl GameState {
    pub fn new(seed: u64) -> Self {
        let nations = INITIAL_NATION_STATE.iter().cloned().collect();
        let countries = COUNTRIES.iter().cloned().collect();
        let initial_state_of_war: HashMap<Side, WarState> = [
            (
                Side::Allies,
                WarState {
                    resources: 0,
                    vp: 0,
                    technologies: Box::new(initial_technologies()),
                },
            ),
            (
                Side::Empires,
                WarState {
                    resources: 0,
                    vp: 0,
                    technologies: Box::new(initial_technologies()),
                },
            ),
        ]
        .iter()
        .cloned()
        .collect();

        GameState {
            current_turn: 1,
            initiative: Side::Empires,
            winner: None,
            russian_revolution: 0,
            nations,
            countries,
            state_of_war: initial_state_of_war,
            seed,
            rng: StdRng::seed_from_u64(seed),
            events_pool: ALL_EVENTS
                .iter()
                .filter(|e| e.year == 1914)
                .cloned()
                .collect(),
            active_events: Vec::new(),
        }
    }

    pub fn tally_resources(&self, pr_for_side: &Side) -> u8 {
        self.nations
            .iter()
            .fold(0, |acc, (nation, status)| match status {
                NationState::AtWar(breakdown) => match self.countries.get(nation) {
                    Some(Country {
                        side, resources, ..
                    }) if side == pr_for_side => {
                        acc + if *nation == Nation::Russia {
                            operational_level(*breakdown) * 2
                        } else {
                            *resources
                        }
                    }
                    _ => acc,
                },
                _ => acc,
            })
    }

    pub fn increase_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        let st = self.state_of_war.get_mut(&side).unwrap();
        st.resources += pr;
        if st.resources > 20 {
            st.resources = 20;
        }
        self
    }

    pub fn roll(&mut self) -> u8 {
        self.rng.gen_range(1..=6)
    }

    pub fn current_year(&self) -> u16 {
        match self.current_turn {
            1 => 1914,
            2..=4 => 1915,
            5..=7 => 1916,
            8..=10 => 1917,
            11..=13 => 1918,
            14 => 1919,
            _ => panic!("Invalid turn"),
        }
    }

    pub fn all_nations_at_war(&self, initiative: Side) -> Vec<Nation> {
        self.nations
            .iter()
            .filter_map(|(nation, status)| match status {
                NationState::AtWar(_) => Some(*nation),
                _ => None,
            })
            .filter(|nation| self.countries.get(nation).unwrap().side == initiative)
            .collect()
    }

    pub fn artillery_bonus(&self, initiative: &Side) -> u8 {
        self.state_of_war
            .get(initiative)
            .unwrap()
            .technologies
            .artillery
    }

    pub fn attack_bonus(&self, initiative: &Side) -> u8 {
        self.state_of_war
            .get(initiative)
            .unwrap()
            .technologies
            .attack
    }

    pub fn defense_bonus(&self, initiative: &Side) -> u8 {
        self.state_of_war
            .get(initiative)
            .unwrap()
            .technologies
            .defense
    }

    fn surrenders(&mut self, to: &Nation) -> HitsResult {
        let side = self.countries.get(to).unwrap().side.other();
        self.state_of_war.get_mut(&side).unwrap().vp += self.countries.get(to).unwrap().vp;
        self.nations.insert(*to, NationState::AtPeace);
        if self.roll() < self.state_of_war.get(&side).unwrap().vp {
            self.winner = Some(side);
            HitsResult::Winner(side)
        } else {
            HitsResult::Surrenders(*to)
        }
    }

    pub(crate) fn draw_events(&mut self) -> Vec<Event> {
        let mut events = Vec::new();
        for _ in 0..3 {
            let idx = self.rng.gen_range(0..self.events_pool.len());
            let event = self.events_pool.remove(idx);
            events.push(event);
        }
        events
    }
    pub(crate) fn new_turn(&mut self) -> &Self {
        let current_turn_year = self.current_year();
        self.current_turn += 1;
        let next_year = self.current_year();
        if next_year != current_turn_year {
            self.events_pool.retain(|event| {
                event.not_after.is_none() || event.not_after.unwrap() > current_turn_year
            });
            self.events_pool.extend(
                ALL_EVENTS
                    .iter()
                    .filter(|event| event.year == next_year)
                    .cloned(),
            );
        }
        self.deactivate_events();
        self
    }

    pub(crate) fn activate_event(&mut self, event: &Event) {
        self.active_events.push(event.activate());
    }

    fn deactivate_events(&mut self) {
        let still_active = self
            .active_events
            .iter()
            .filter(|active_event| !(active_event.deactivation)(self));
        self.active_events = still_active.cloned().collect();
    }

    fn adjust_attack_bonus(&self, attack_bonus: u8, from: &Nation, to: &Nation) -> u8 {
        let event_bonus = if self.active_events.iter().any(|event| {
            event.event.event_id == 4
                && ((from == &Nation::France && to == &Nation::Germany)
                    || (from == &Nation::Germany && to == &Nation::France))
        }) {
            1
        } else {
            0
        };
        event_bonus + attack_bonus
    }
}

impl Display for GameState {
    /// TODO: take care of writeln! result
    #[allow(unused_must_use)]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "Turn: {}", self.current_turn);
        writeln!(f, "Initiative: {}", self.initiative);
        if let Some(winner) = self.winner {
            writeln!(f, "Winner: {}", winner);
        }
        writeln!(f, "Russian Revolution: {}", self.russian_revolution);
        writeln!(f, "Breakdown:");
        for (nation, status) in self.nations.iter() {
            writeln!(f, "\t{}: {}", nation, status);
        }
        writeln!(f, "State of War:");
        for (side, war_state) in self.state_of_war.iter() {
            writeln!(f, "\t{}:", side);
            writeln!(f, "\t\tResources: {}", war_state.resources);
            writeln!(f, "\t\tVP: {}", war_state.vp);
            writeln!(f, "\t\tTechnologies: {}", war_state.technologies);
        }
        Ok(())
    }
}

#[cfg(test)]
mod game_state_tests {

    use super::HitsResult::*;
    use crate::{fixtures::EngineBuilder, Nation::*, NationState::*, Side::*, ALL_EVENTS};

    #[test]
    fn nation_surrenders_when_brought_to_0_then_increase_vp_of_other_side() {
        let mut engine = EngineBuilder::new(14) // die roll = 6
            .with_nation(France, AtWar(4))
            .build();
        let result = engine.apply_hits(&France, 4);

        assert_eq!(Surrenders(France), result);
        assert_eq!(6, engine.state.state_of_war.get(&Empires).unwrap().vp);
        assert_eq!(AtPeace, engine.state.nations.get(&France).unwrap().clone());
    }

    #[test]
    fn side_wins_if_die_roll_lower_than_vp_given_nation_surrenders() {
        let mut engine = EngineBuilder::new(11) // die roll = 2
            .with_nation(France, AtWar(4))
            .build();
        let result = engine.apply_hits(&France, 4);

        assert_eq!(Some(Empires), engine.state.winner);
        assert_eq!(Winner(Empires), result);
    }

    #[test]
    fn breakdown_returns_hits_lost() {
        let mut engine = EngineBuilder::new(11).with_nation(France, AtWar(4)).build();

        assert_eq!(Hits(France, 2), engine.apply_hits(&France, 2));
    }

    #[test]
    fn breakdown_does_not_inflict_hits_given_nation_is_not_at_war() {
        let mut engine = EngineBuilder::new(11).build();

        assert_eq!(NationNotAtWar(Italy), engine.apply_hits(&Italy, 2));
    }

    #[test]
    fn on_new_turn_remove_inactive_events() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Race to the sea"
        engine.activate_event(&ALL_EVENTS[3]);
        engine.new_turn();

        assert_eq!(0, engine.state.active_events.len());
    }
}
