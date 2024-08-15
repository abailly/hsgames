use crate::event::Event;
use crate::event::ARMISTICE;
use crate::events::*;
use crate::logic::*;
use crate::side::*;
use crate::state::StateChange::*;
use crate::state::*;
use crate::TechEffects;
use crate::TechnologyType;
use crate::ALLIES_TECHNOLOGIES;
use crate::DEFAULT_INITIATIVE;
use crate::EMPIRE_TECHNOLOGIES;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::mem::swap;

#[derive(Debug, Clone)]
pub struct GameEngine {
    pub(crate) state: GameState,
    logic: Box<dyn GameLogic>,
    played_events: Vec<ActiveEvent>,
}

impl GameEngine {
    pub fn new(seed: u64) -> Self {
        GameEngine {
            state: GameState::new(seed),
            logic: Box::new(default_game_logic()),
            played_events: Vec::new(),
        }
    }

    pub fn with_state(state: GameState) -> GameEngine {
        GameEngine {
            state,
            logic: Box::new(default_game_logic()),
            played_events: Vec::new(),
        }
    }

    pub fn game_ends(self: &GameEngine) -> bool {
        self.state.game_ends()
    }

    pub fn collect_resources(&mut self) {
        self.logic.collect_resources(&mut self.state);
    }

    pub fn reduce_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        self.logic.reduce_pr(&mut self.state, &side, pr);
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
        let available_resources = self.state.resources_for(&nation.side());
        let nation_state = self.state.nations.get_mut(&nation).unwrap();
        let maximum_breakdown = nation.maximum_breakdown();
        let current_breakdown = nation_state.breakdown();

        let (spent, reinforcement) =
            (1..=(pr + 1)).fold((0, 0), |(spent, reinforcement), resource| {
                let new_spent = spent + resource;
                if new_spent <= pr
                    && reinforcement + current_breakdown < maximum_breakdown
                    && new_spent <= available_resources
                {
                    (new_spent, reinforcement + 1)
                } else {
                    (spent, reinforcement)
                }
            });

        nation_state.reinforce(reinforcement);
        self.reduce_pr(nation.side(), spent);

        if nation == Nation::Russia && reinforcement > 0 {
            self.track_russian_revolution(reinforcement);
        }

        self
    }

    fn track_russian_revolution(&mut self, reinforcement: u8) {
        let revolution_increase = (0..reinforcement)
            .map(|_| self.roll())
            .filter(|&die| die == 1)
            .count() as u8;
        self.state.russian_revolution += revolution_increase;
        if self.state.russian_revolution >= 6 {
            self.state.surrenders(&Nation::Russia);
        }
    }

    pub(crate) fn apply_hits(&mut self, to: &Nation, hits: u8) -> HitsResult {
        self.logic.apply_hits(&mut self.state, to, hits)
    }

    pub(crate) fn draw_events(&mut self) -> Vec<Event> {
        self.state.draw_events()
    }

    pub(crate) fn resolve_offensive(&mut self, offensive: &Offensive) -> HitsResult {
        let (artillery_bonus, attack_bonus, defense_malus, _) = self.compute_bonus(offensive);

        let dice: Vec<u8> = self.roll_offensive_dice(offensive.pr);
        let artillery_dice: Vec<u8> = self.roll_artillery_dice(artillery_bonus);

        let attack_hits = self.evaluate_attack_hits(attack_bonus, defense_malus, offensive, &dice);

        let artillery_hits = self.evaluate_artillery_hits(offensive, &artillery_dice);

        self.reduce_pr(offensive.initiative, offensive.pr);
        self.apply_hits(&offensive.to, attack_hits + artillery_hits)
    }

    fn evaluate_artillery_hits(&mut self, offensive: &Offensive, artillery_dice: &Vec<u8>) -> u8 {
        self.logic
            .evaluate_artillery_hits(&self.state, offensive, artillery_dice)
    }

    pub fn evaluate_attack_hits(
        &mut self,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice: &Vec<u8>,
    ) -> u8 {
        self.logic.evaluate_attack_hits(
            &mut self.state,
            attack_bonus,
            defense_malus,
            offensive,
            dice,
        )
    }

    pub fn roll_artillery_dice(&mut self, artillery_bonus: u8) -> Vec<u8> {
        self.logic
            .roll_artillery_dice(&mut self.state, artillery_bonus)
    }

    pub fn roll_offensive_dice(&mut self, pr: u8) -> Vec<u8> {
        self.logic.roll_offensive_dice(&mut self.state, pr)
    }

    pub fn compute_bonus(&mut self, offensive: &Offensive) -> TechEffects {
        self.logic.compute_bonus(&self.state, offensive)
    }

    pub fn uboot_losses(&mut self, bonus: u8) -> StateChange {
        self.logic.uboot_losses(&mut self.state, bonus)
    }

    pub(crate) fn new_turn(&mut self) -> &mut Self {
        self.logic.new_turn(&mut self.state);
        self
    }

    pub(crate) fn play_events(&mut self, event: &Event) {
        let active_event = self.play(event);
        self.played_events.push(active_event);
    }

    fn play(&mut self, event: &Event) -> ActiveEvent {
        match event.event_id {
            4 => self.activate_event(RaceToTheSea::new),
            5 => self.activate_event(ShellCrisis::new),
            6 => self.activate_event(Gas::new),
            7 => self.activate_event(VonLettowInAfrica::new),
            8 => self.activate_event(Gallipoli::new),
            9 => self.activate_event(SeparatePeace::new),
            10 => {
                self.state
                    .nations
                    .insert(Nation::Italy, NationState::AtWar(5));
            }
            11 => {
                self.state
                    .nations
                    .insert(Nation::Bulgaria, NationState::AtWar(3));
            }
            12 => self.activate_event(LusitaniaSunk::new),
            14 => match self.roll() {
                1 => {
                    self.increase_pr(Side::Empires, 3);
                }
                5 => {
                    self.activate_event(GermanFleetDefeated::new);
                }
                6 => {
                    self.activate_event(GermanFleetDestroyed::new);
                }
                _ => {}
            },
            15 => self.activate_event(AustrianOffensive::new),
            16 => self.activate_event(WoodrowWilson::new),
            19 => self.activate_event(BrusilovOffensive::new),
            20 => {
                self.state
                    .nations
                    .insert(Nation::Romania, NationState::AtWar(3));
            }
            22 => self.activate_event(Mutinies::new),
            23 => self.activate_event(GazaOffensive::new),
            24 => {
                self.reduce_pr(Side::Empires, 2);
            }
            25 => self.activate_event(AustrianOffensive::new),
            26 => self.activate_event(UBoot::new),
            27 => self.activate_event(FlyingCircus::new),
            28 => {
                self.state
                    .nations
                    .insert(Nation::Greece, NationState::AtWar(3));
            }
            29 => self.activate_event(ZimmermanTelegram::new),
            31 => {
                self.state
                    .nations
                    .insert(Nation::Russia, NationState::AtPeace);
            }
            32 => self.activate_event(Friedensturm::new),
            33 => self.activate_event(UnifiedCommand::new),
            35 => self.activate_event(BattleOfMegiddo::new),
            36 => {
                if let Some(NationState::AtWar(_)) = self.state.nations.get(&Nation::Greece) {
                    self.activate_event(SalonikiExpedition::new)
                }
            }
            37 => {
                let all_nations = self.state.nations.keys().cloned().collect::<Vec<Nation>>();
                for nation in all_nations.iter() {
                    self.apply_hits(nation, 1);
                }
            }
            38 => {
                self.state.add_event(ARMISTICE);
            }
            42 => {
                self.state.end_game_this_turn = true;
            }
            _ => {}
        }
        let active_event = ActiveEvent {
            event: event.clone(),
            deactivation: |_game| true, // by default, events last for one turn
        };
        self.logic.event_activated(&active_event);
        active_event
    }

    fn activate_event<T>(&mut self, new: fn(Box<dyn GameLogic>) -> T)
    where
        T: GameLogic + 'static,
    {
        let mut previous: Box<dyn GameLogic> = Box::new(DummyLogic::new());
        swap(&mut previous, &mut self.logic);
        self.logic = Box::new((new)(previous));
    }

    pub(crate) fn blockade_effect(&mut self, bonus: u8) -> StateChange {
        self.logic.blockade_effect(&mut self.state, bonus)
    }

    pub(crate) fn improve_technology(&mut self, initiative: &Side, tech: &TechnologyType) {
        self.state
            .state_of_war
            .get_mut(initiative)
            .unwrap()
            .technologies
            .as_mut()
            .improve(tech);
    }

    pub(crate) fn winner(&self) -> Side {
        self.state.winner()
    }

    pub(crate) fn apply_change(&mut self, change: &StateChange) {
        self.state.apply_change(change);
    }

    pub(crate) fn set_phase(&mut self, phase: Phase) {
        self.state.set_phase(phase)
    }

    pub(crate) fn determine_initiative(&mut self, allies_pr: u8, empires_pr: u8) {
        let allies_initiative = allies_pr + self.roll();
        let empires_initiative = empires_pr + self.roll();

        self.state.initiative = match allies_initiative.cmp(&empires_initiative) {
            Ordering::Greater => Side::Allies,
            Ordering::Less => Side::Empires,
            Ordering::Equal => DEFAULT_INITIATIVE[self.state.current_turn as usize - 1],
        };

        self.reduce_pr(Side::Allies, allies_pr);
        self.reduce_pr(Side::Empires, empires_pr);
    }

    pub(crate) fn valuation(&self) -> f64 {
        self.state.valuation()
    }

    pub(crate) fn try_improve_technology(
        &mut self,
        initiative: Side,
        tech: TechnologyType,
        pr_spent: u8,
    ) -> TechnologyImprovement {
        let die = self.roll();
        let year = self.current_year();
        let techs = &mut self
            .state
            .state_of_war
            .get_mut(&initiative)
            .unwrap()
            .technologies;
        let current_tech_level = techs.value(&tech);
        let technologies_track = match initiative {
            Side::Allies => &ALLIES_TECHNOLOGIES,
            Side::Empires => &EMPIRE_TECHNOLOGIES,
        };

        if current_tech_level == 4 {
            return TechnologyImprovement::NoMoreTechnologyImprovement(tech, current_tech_level);
        }

        if let Some(technology) = &technologies_track[tech.index()][current_tech_level as usize] {
            if year >= technology.date {
                if die + pr_spent > technology.min_dice_unlock {
                    self.improve_technology(&initiative, &tech);
                    self.reduce_pr(initiative, pr_spent);
                    TechnologyImprovement::ImprovedTechnology(tech, pr_spent)
                } else {
                    self.reduce_pr(initiative, pr_spent);
                    TechnologyImprovement::FailedTechnology(tech, pr_spent)
                }
            } else {
                TechnologyImprovement::TechnologyNotAvailable(
                    technology.name.to_string(),
                    technology.date,
                    year,
                )
            }
        } else {
            TechnologyImprovement::NoMoreTechnologyImprovement(tech, current_tech_level)
        }
    }
}

#[derive(Clone, Debug)]
struct DefaultGameLogic {}

impl GameLogic for DefaultGameLogic {
    fn collect_resources(&mut self, state: &mut GameState) {
        state.lafayette = state.lafayette.map(|l| l + 1);
        state.increase_pr(
            Side::Allies,
            state.tally_resources(&Side::Allies) + state.lafayette.unwrap_or(0),
        );
        state.increase_pr(Side::Empires, state.tally_resources(&Side::Empires));
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        let max_attacker_tech_level = state.countries.get(&offensive.from).unwrap().max_tech_level;
        let max_defender_tech_level = state.countries.get(&offensive.to).unwrap().max_tech_level;

        let artillery_bonus = state
            .artillery_bonus(&offensive.initiative)
            .min(max_attacker_tech_level);
        let attack_bonus = state
            .attack_bonus(&offensive.initiative)
            .min(max_attacker_tech_level) as i8;

        let defense_malus = state
            .defense_bonus(&offensive.initiative.other())
            .min(max_defender_tech_level) as i8;
        (artillery_bonus, attack_bonus, defense_malus, 0)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        (0..num).map(|_| state.roll()).collect()
    }

    fn roll_artillery_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        (0..num).map(|_| state.roll()).collect()
    }

    fn evaluate_attack_hits(
        &mut self,
        state: &mut GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        {
            let attack_country = |die: u8| {
                return die as i8 + attack_bonus - defense_malus
                    >= state.countries.get(&offensive.from).unwrap().attack_factor as i8;
            };
            let attack_hits = dice_roll
                .iter()
                .map(|die| attack_country(*die))
                .filter(|hit| *hit)
                .count() as u8;
            attack_hits
        }
    }

    fn evaluate_artillery_hits(
        &mut self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        {
            let bomb_country =
                |die: u8| return die >= state.countries.get(&offensive.from).unwrap().attack_factor;
            let artillery_hits = dice_roll
                .iter()
                .map(|die| bomb_country(*die))
                .filter(|hit| *hit)
                .count() as u8;
            artillery_hits
        }
    }

    fn reduce_pr(&mut self, state: &mut GameState, side: &Side, pr: u8) {
        {
            let st = state.state_of_war.get_mut(side).unwrap();
            if st.resources >= pr {
                st.resources -= pr;
            }
        };
    }

    fn apply_hits(&mut self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        if let NationState::AtWar(breakdown) = state.nations.get_mut(nation).unwrap() {
            if hits >= *breakdown {
                state.surrenders(nation)
            } else {
                *breakdown -= hits;
                HitsResult::Hits(*nation, hits)
            }
        } else {
            HitsResult::NationNotAtWar(*nation)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        let current_turn_year = state.current_year();
        state.current_turn += 1;
        if !state.game_ends() {
            let next_year = state.current_year();
            if next_year != current_turn_year {
                state.new_year(current_turn_year, next_year);
            }
        }
    }

    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> StateChange {
        let die = state.roll() + bonus;
        let loss = match die {
            1..=4 => 0,
            5 => 2,
            _ => 4,
        };
        MoreChanges(vec![
            ChangeResources {
                side: Side::Allies,
                pr: -loss,
            },
            ChangeResources {
                side: Side::Empires,
                pr: -(bonus as i8),
            },
        ])
    }

    fn blockade_effect(&mut self, state: &mut GameState, bonus: u8) -> StateChange {
        let die = state.roll() + bonus;
        let gain = match die {
            1 => 3,
            2 => 1,
            _ => 0,
        };

        MoreChanges(vec![
            ChangeResources {
                side: Side::Allies,
                pr: -(bonus as i8),
            },
            ChangeResources {
                side: Side::Empires,
                pr: gain,
            },
        ])
    }
}

fn default_game_logic() -> impl GameLogic {
    DefaultGameLogic {}
}

#[cfg(test)]
mod engine_test {
    use crate::{
        event::ALL_EVENTS,
        fixtures::EngineBuilder,
        Nation, NationState,
        Side::{self, *},
    };

    #[test]
    fn played_events_stay_between_turns() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Race to the sea"
        engine.play_events(&ALL_EVENTS[3]);
        engine.new_turn();

        assert_eq!(1, engine.played_events.len());
    }

    #[test]
    fn lafayette_increases_by_1_every_turn() {
        let mut engine = EngineBuilder::new(14).build();

        engine.state.lafayette = Some(0);
        engine.collect_resources();

        assert_eq!(15, engine.state.resources_for(&Allies));
    }

    #[test]
    fn when_turn_reaches_14_end_the_game() {
        let mut engine = EngineBuilder::new(14).on_turn(14).build();

        engine.new_turn();

        assert!(engine.game_ends());
    }

    #[test]
    fn when_reinforcing_russia_and_die_is_1_increase_revolution_track_by_one() {
        let mut engine = EngineBuilder::new(2) // die roll = 1
            .with_resources(Side::Allies, 1)
            .with_nation(Nation::Russia, NationState::AtWar(5))
            .build();

        engine.reinforce(Nation::Russia, 1);

        assert_eq!(
            6,
            engine
                .state
                .nations
                .get(&Nation::Russia)
                .unwrap()
                .breakdown()
        );
        assert_eq!(1, engine.state.russian_revolution);
    }

    #[test]
    fn when_reinforcing_russia_roll_as_many_dice_as_reinforcement() {
        let mut engine = EngineBuilder::new(7) // die roll = 3 1 1
            .with_resources(Allies, 10)
            .with_nation(Nation::Russia, NationState::AtWar(2))
            .build();

        engine.reinforce(Nation::Russia, 6);

        assert_eq!(
            5,
            engine
                .state
                .nations
                .get(&Nation::Russia)
                .unwrap()
                .breakdown()
        );
        assert_eq!(2, engine.state.russian_revolution);
    }

    #[test]
    fn russia_surrenders_when_revolution_track_reaches_6() {
        let mut engine = EngineBuilder::new(7) // die roll = 3 1 1
            .with_resources(Allies, 10)
            .with_russian_revolution(4)
            .with_nation(Nation::Russia, NationState::AtWar(2))
            .build();

        engine.reinforce(Nation::Russia, 6);

        assert_eq!(
            &NationState::AtPeace,
            engine.state.nations.get(&Nation::Russia).unwrap()
        );
        assert_eq!(3, engine.state.state_of_war.get(&Empires).unwrap().vp);
        assert_eq!(6, engine.state.russian_revolution);
    }
}
