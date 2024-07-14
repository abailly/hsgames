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

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct ActiveEvent {
    pub event: Event,
    pub duration: u8,
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

    pub fn reduce_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        let st = self.state_of_war.get_mut(&side).unwrap();
        if st.resources >= pr {
            st.resources -= pr;
        }
        self
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

    pub(crate) fn reinforce(&mut self, nation: Nation, pr: u8) -> &Self {
        let nation_state = self.nations.get_mut(&nation).unwrap();
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

    pub(crate) fn breakdown(&mut self, to: &Nation, hits: u8) -> HitsResult {
        if let NationState::AtWar(breakdown) = self.nations.get_mut(to).unwrap() {
            if hits >= *breakdown {
                self.surrenders(to)
            } else {
                *breakdown -= hits;
                HitsResult::Hits(*to, hits)
            }
        } else {
            HitsResult::NationNotAtWar(*to)
        }
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

    pub(crate) fn resolve_offensive(
        &mut self,
        initiative: Side,
        from: Nation,
        to: Nation,
        pr: u8,
    ) -> HitsResult {
        let max_attacker_tech_level = self.countries.get(&from).unwrap().max_tech_level;
        let max_defender_tech_level = self.countries.get(&to).unwrap().max_tech_level;

        let artillery_bonus = self
            .artillery_bonus(&initiative)
            .min(max_attacker_tech_level);
        let attack_bonus = self.adjust_attack_bonus(
            self.attack_bonus(&initiative).min(max_attacker_tech_level),
            &from,
            &to,
        );
        let defense_malus = self
            .defense_bonus(&initiative.other())
            .min(max_defender_tech_level);

        let dice: Vec<u8> = (0..pr).map(|_| self.roll()).collect();
        let artillery_dice: Vec<u8> = (0..artillery_bonus).map(|_| self.roll()).collect();

        let attack_country = |die: u8| {
            return die + attack_bonus - defense_malus
                >= self.countries.get(&from).unwrap().attack_factor;
        };

        let bomb_country = |die: u8| return die >= self.countries.get(&from).unwrap().attack_factor;

        let attack_hits = dice
            .iter()
            .map(|die| attack_country(*die))
            .filter(|hit| *hit)
            .count() as u8;
        let artillery_hits = artillery_dice
            .iter()
            .map(|die| bomb_country(*die))
            .filter(|hit| *hit)
            .count() as u8;

        self.reduce_pr(initiative, pr);
        self.breakdown(&to, attack_hits + artillery_hits)
    }

    pub(crate) fn new_turn(&mut self) -> &Self {
        let current_turn_year = self.current_year();
        self.current_turn += 1;
        let next_year = self.current_year();
        if next_year != current_turn_year {
            self.events_pool.retain(|event| {
                event.not_after.is_none() || event.not_after.unwrap() > current_turn_year
            });
            println!("pool {:?}", &self.events_pool);
            self.events_pool.extend(
                ALL_EVENTS
                    .iter()
                    .filter(|event| event.year == next_year)
                    .cloned(),
            );
        }
        println!("pool {:?}", &self.events_pool);
        self
    }

    pub(crate) fn activate_event(&mut self, event: ActiveEvent) {
        self.active_events.push(event);
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
    use crate::{fixtures::StateBuilder, Nation::*, NationState::*, Side::*};

    #[test]
    fn nation_surrenders_when_brought_to_0_then_increase_vp_of_other_side() {
        let mut state = StateBuilder::new(14) // die roll = 6
            .with_nation(France, AtWar(4))
            .build();

        let result = state.breakdown(&France, 4);

        assert_eq!(Surrenders(France), result);
        assert_eq!(6, state.state_of_war.get(&Empires).unwrap().vp);
        assert_eq!(AtPeace, state.nations.get(&France).unwrap().clone());
    }

    #[test]
    fn side_wins_if_die_roll_lower_than_vp_given_nation_surrenders() {
        let mut state = StateBuilder::new(11) // die roll = 2
            .with_nation(France, AtWar(4))
            .build();

        let result = state.breakdown(&France, 4);

        assert_eq!(Some(Empires), state.winner);
        assert_eq!(Winner(Empires), result);
    }

    #[test]
    fn breakdown_returns_hits_lost() {
        let mut state = StateBuilder::new(11).with_nation(France, AtWar(4)).build();

        assert_eq!(Hits(France, 2), state.breakdown(&France, 2));
    }

    #[test]
    fn breakdown_does_not_inflict_hits_given_nation_is_not_at_war() {
        let mut state = StateBuilder::new(11).build();

        assert_eq!(NationNotAtWar(Italy), state.breakdown(&Italy, 2));
    }
}
