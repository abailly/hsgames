use crate::logic::*;
use crate::side::*;
use crate::state::*;
use crate::TechEffects;

impl GameLogic for RaceToTheSea {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        if self.active
            && (offensive.from == Nation::France && offensive.to == Nation::Germany
                || (offensive.from == Nation::Germany && offensive.to == Nation::France))
        {
            let (artillery_bonus, attack_bonus, defense_malus, air) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus + 1, defense_malus, air)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct RaceToTheSea {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl RaceToTheSea {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        RaceToTheSea {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl<T: GameLogic> GameLogic for ShellCrisis<T> {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        if self.active && offensive.from == Nation::France {
            let (artillery_bonus, attack_bonus, defense_malus, air) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus - 1, defense_malus, air)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct ShellCrisis<T: GameLogic> {
    pub active: bool,
    pub previous: Box<T>,
}

impl<T: GameLogic> ShellCrisis<T> {
    pub fn new(previous: T) -> Self {
        ShellCrisis {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for Gas {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }

    fn roll_artillery_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        let roll_artillery_dice = self.previous.roll_artillery_dice(state, num);
        if let Some(offensive) = &mut self.offensive {
            if offensive.from == Nation::Germany {
                return roll_artillery_dice.into_iter().map(|die| die + 1).collect();
            }
        }
        roll_artillery_dice
    }
}

pub struct Gas {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
    pub offensive: Option<Offensive>,
}

impl Gas {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        Gas {
            active: true,
            offensive: None,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for VonLettowInAfrica {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        if state.operational_level(&Nation::GermanAfrica) == 0 {
            self.active = false;
        }
        if self.active {
            self.offensive = Some(offensive.clone());
        } else {
            self.offensive = None;
        }
        self.previous.compute_bonus(state, offensive)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        if let Some(offensive) = &self.offensive {
            if offensive.from == Nation::GermanAfrica {
                return self.previous.roll_offensive_dice(state, num + 1);
            }
        };
        self.previous.roll_offensive_dice(state, num)
    }
}

pub struct VonLettowInAfrica {
    pub offensive: Option<Offensive>,
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl VonLettowInAfrica {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        VonLettowInAfrica {
            offensive: None,
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for Gallipoli {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn collect_resources(&mut self, state: &mut GameState) {
        self.previous.collect_resources(state);
        if self.active {
            let die = state.roll();
            self.reduce_pr(state, &Side::Allies, die);
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.previous.new_turn(state);
        self.active = false;
    }
}

pub struct Gallipoli {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl Gallipoli {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        Gallipoli {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for GermanFleetDefeated {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> u8 {
        let die = (state.roll() - 1 + bonus).max(1);
        match die {
            1..=4 => 0,
            5 => 2,
            _ => 4,
        }
    }
}

pub struct GermanFleetDefeated {
    pub previous: Box<dyn GameLogic>,
}

impl GermanFleetDefeated {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        GermanFleetDefeated {
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for GermanFleetDestroyed {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn blockade_effect(&mut self, _state: &mut GameState, _bonus: u8) -> u8 {
        0
    }
}

pub struct GermanFleetDestroyed {
    pub previous: Box<dyn GameLogic>,
}

impl GermanFleetDestroyed {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        GermanFleetDestroyed {
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for SeparatePeace {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.previous.new_turn(state);
        self.active = false;
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        if let Some(offensive) = &self.offensive {
            if self.active
                && (offensive.from == Nation::Germany || offensive.from == Nation::AustriaHungary)
                && offensive.to == Nation::Russia
            {
                return self.previous.roll_offensive_dice(state, num + 1);
            }
        };

        self.previous.roll_offensive_dice(state, num)
    }
}

pub struct SeparatePeace {
    pub offensive: Option<Offensive>,
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl SeparatePeace {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        SeparatePeace {
            offensive: None,
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for AustrianOffensive {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        if self.active && offensive.from == Nation::AustriaHungary && offensive.to == Nation::Italy
        {
            let (artillery_bonus, attack_bonus, defense_malus, air) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus + 1, defense_malus, air)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct AustrianOffensive {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl AustrianOffensive {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        AustrianOffensive {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for WoodrowWilson {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> u8 {
        if self.uboot_played {
            self.previous.uboot_losses(state, bonus)
        } else {
            0
        }
    }

    fn event_activated(&mut self, event: &ActiveEvent) {
        if event.event.event_id == 26 {
            self.uboot_played = true;
        }
        self.previous.event_activated(event);
    }
}

pub struct WoodrowWilson {
    pub uboot_played: bool,
    pub previous: Box<dyn GameLogic>,
}

impl WoodrowWilson {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        WoodrowWilson {
            uboot_played: false,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for BrusilovOffensive {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        let add_to_die = if let Some(offensive) = &self.offensive {
            if self.applies(offensive) {
                1
            } else {
                0
            }
        } else {
            0
        };
        self.previous
            .roll_offensive_dice(state, num)
            .into_iter()
            .map(|die| (die + add_to_die).min(6))
            .collect()
    }

    fn evaluate_attack_hits(
        &mut self,
        state: &mut GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        let hits = self.previous.evaluate_attack_hits(
            state,
            attack_bonus,
            defense_malus,
            offensive,
            dice_roll,
        );
        if let Some(offensive) = &self.offensive {
            if self.applies(offensive) && hits < dice_roll.len() as u8 {
                self.reduce_pr(state, &Side::Allies, dice_roll.len() as u8 - hits);
            }
        }
        hits
    }
    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct BrusilovOffensive {
    pub offensive: Option<Offensive>,
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl BrusilovOffensive {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        BrusilovOffensive {
            offensive: None,
            active: true,
            previous: Box::new(previous),
        }
    }

    fn applies(&self, offensive: &Offensive) -> bool {
        self.active && offensive.from == Nation::Russia && offensive.to == Nation::AustriaHungary
    }
}

impl GameLogic for Mutinies {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        let remove_from_dice = if let Some(offensive) = &self.offensive {
            if self.active && offensive.from == Nation::France {
                1
            } else {
                0
            }
        } else {
            0
        };
        self.previous
            .roll_offensive_dice(state, (num - remove_from_dice).max(0))
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct Mutinies {
    pub offensive: Option<Offensive>,
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl Mutinies {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        Mutinies {
            offensive: None,
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for GazaOffensive {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        let dice = self.previous.roll_offensive_dice(state, num);
        if let Some(offensive) = &self.offensive {
            if self.applies(offensive) {
                self.dice = Some(dice.clone());
            }
        };
        dice
    }

    fn evaluate_attack_hits(
        &mut self,
        state: &mut GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        let hits = self.previous.evaluate_attack_hits(
            state,
            attack_bonus,
            defense_malus,
            offensive,
            dice_roll,
        );
        if let Some(offensive) = &self.offensive {
            if self.applies(offensive) {
                let new_dice = (0..hits).map(|_| state.roll()).collect();
                return self.previous.evaluate_attack_hits(
                    state,
                    attack_bonus,
                    defense_malus,
                    offensive,
                    &new_dice,
                );
            }
        }
        hits
    }
    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct GazaOffensive {
    pub dice: Option<Vec<u8>>,
    pub offensive: Option<Offensive>,
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl GazaOffensive {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        GazaOffensive {
            dice: None,
            offensive: None,
            active: true,
            previous: Box::new(previous),
        }
    }

    fn applies(&self, offensive: &Offensive) -> bool {
        self.active && offensive.from == Nation::Egypt && offensive.to == Nation::OttomanEmpire
    }
}

impl GameLogic for UBoot {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> u8 {
        let die = state.roll() + 1 + bonus;
        let loss = match die {
            1..=4 => 0,
            5 => 2,
            _ => 4,
        };
        self.reduce_pr(state, &Side::Empires, bonus);
        loss
    }
}

pub struct UBoot {
    pub previous: Box<dyn GameLogic>,
}

impl UBoot {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        UBoot {
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for FlyingCircus {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        if self.number_of_turns_remaining > 0 && (offensive.initiative == Side::Empires) {
            let (artillery_bonus, attack_bonus, defense_malus, air_power) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus, defense_malus, air_power + 2)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.number_of_turns_remaining -= 1;
        self.previous.new_turn(state);
    }
}

pub struct FlyingCircus {
    pub number_of_turns_remaining: u8,
    pub previous: Box<dyn GameLogic>,
}

impl FlyingCircus {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        FlyingCircus {
            number_of_turns_remaining: 2,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for LusitaniaSunk {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn event_activated(&mut self, event: &ActiveEvent) {
        if event.event.event_id == 29 {
            self.lafayette_nous_voila = true;
        }
        self.previous.event_activated(event);
    }

    fn new_turn(&mut self, state: &mut GameState) {
        if state.lafayette.is_none() && self.lafayette_nous_voila {
            state.lafayette = Some(0);
        };
        self.previous.new_turn(state);
    }
}

pub struct LusitaniaSunk {
    pub lafayette_nous_voila: bool,
    pub previous: Box<dyn GameLogic>,
}

impl LusitaniaSunk {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        LusitaniaSunk {
            lafayette_nous_voila: false,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for ZimmermanTelegram {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn event_activated(&mut self, event: &ActiveEvent) {
        if event.event.event_id == 12 {
            self.lafayette_nous_voila = true;
        }
        self.previous.event_activated(event);
    }

    fn new_turn(&mut self, state: &mut GameState) {
        if state.lafayette.is_none() && self.lafayette_nous_voila {
            state.lafayette = Some(0);
        };
        self.previous.new_turn(state);
    }
}

pub struct ZimmermanTelegram {
    pub lafayette_nous_voila: bool,
    pub previous: Box<dyn GameLogic>,
}

impl ZimmermanTelegram {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        ZimmermanTelegram {
            lafayette_nous_voila: false,
            previous: Box::new(previous),
        }
    }
}

#[cfg(test)]
mod game_events_tests {

    use crate::{
        event::ALL_EVENTS, fixtures::EngineBuilder, Nation::*, NationState, Offensive, Side::*,
    };

    #[test]
    fn shell_crisis_event_inflicts_minus_1_to_france_attacks() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Shells crisis"
        engine.play_events(&ALL_EVENTS[4]);
        let (_, att, _, _) = engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: France,
            to: Germany,
            pr: 2,
        });

        assert_eq!(-1, att);
    }

    #[test]
    fn gas_event_adds_1_to_germany_artillery_rolls() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Gas!"
        engine.play_events(&ALL_EVENTS[5]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: Germany,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_artillery_dice(2);

        assert_eq!(vec![3, 3], dice);
    }

    #[test]
    fn gas_event_does_not_add_to_austria_artillery_rolls() {
        let mut engine = EngineBuilder::new(11).build();

        engine.play_events(&ALL_EVENTS[5]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: AustriaHungary,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_artillery_dice(2);

        assert_eq!(vec![2, 2], dice);
    }

    #[test]
    fn von_lettow_adds_one_dice_for_offensive_in_africa() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Von Lettow in Africa"
        engine.play_events(&ALL_EVENTS[6]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: GermanAfrica,
            to: FrenchAfrica,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![2, 2, 4], dice);
    }

    #[test]
    fn von_lettow_does_node_add_die_once_operational_level_falls_to_0() {
        let mut engine = EngineBuilder::new(11)
            .with_nation(GermanAfrica, NationState::AtWar(1))
            .build();

        // activate "Von Lettow in Africa"
        engine.play_events(&ALL_EVENTS[6]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: GermanAfrica,
            to: FrenchAfrica,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![2, 2], dice);
    }

    #[test]
    fn von_lettow_does_not_add_to_offensive_outside_africa() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Von Lettow in Africa"
        engine.play_events(&ALL_EVENTS[6]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: AustriaHungary,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![2, 2], dice);
    }

    #[test]
    fn gallipoli_reduces_pr_by_1_die_for_allies() {
        let mut engine = EngineBuilder::new(11).build();

        engine.play_events(&ALL_EVENTS[7]);
        engine.collect_resources();

        assert_eq!(12, engine.state.resources_for(&Allies));
    }

    #[test]
    fn separate_peace_adds_1_die_to_offensive_from_germany_to_russia() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[8]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: Germany,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![3, 3, 5], dice);
    }

    #[test]
    fn separate_peace_adds_1_die_to_offensive_from_austria_to_russia() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[8]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: AustriaHungary,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![3, 3, 5], dice);
    }

    #[test]
    fn separate_peace_doesn_not_add_die_to_offensive_from_turkey_to_russia() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[8]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: OttomanEmpire,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![3, 3], dice);
    }

    #[test]
    fn italy_can_enter_war_with_event() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[9]);

        assert_eq!(
            NationState::AtWar(5),
            *engine.state.nations.get(&Italy).unwrap()
        );
    }

    #[test]
    fn bulgaria_can_enter_war_with_event() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[10]);

        assert_eq!(
            NationState::AtWar(3),
            *engine.state.nations.get(&Bulgaria).unwrap()
        );
    }

    #[test]
    fn battle_of_jutland_adds_3_pr_to_empires_on_roll_of_1() {
        let mut engine = EngineBuilder::new(6).build(); // die roll = 1

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(3, engine.state.resources_for(&Empires),);
    }

    #[test]
    fn battle_of_jutland_on_roll_of_5_inflicts_penalty_of_1_to_uboot_rolls() {
        let mut engine = EngineBuilder::new(116).with_resources(Allies, 6).build(); // die roll = 5 , 6

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(2, engine.uboot_losses(0));
    }

    #[test]
    fn battle_of_jutland_on_roll_of_3_has_no_effect() {
        let mut engine = EngineBuilder::new(30).with_resources(Allies, 6).build(); // die roll = 3 , 6

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(4, engine.uboot_losses(0));
    }

    #[test]
    fn battle_of_jutland_on_roll_of_6_cancels_blockade_roll() {
        let mut engine = EngineBuilder::new(22).with_resources(Allies, 6).build(); // die roll = 6, 1

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(0, engine.blockade_effect(1));
        assert_eq!(6, engine.state.resources_for(&Allies));
    }

    #[test]
    fn trentin_offensive_adds_1_to_austria_attack_on_italy() {
        let mut engine = EngineBuilder::new(11).build();

        engine.play_events(&ALL_EVENTS[14]);
        let (_, att, _, _) = engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: AustriaHungary,
            to: Italy,
            pr: 2,
        });

        assert_eq!(1, att);
    }

    #[test]
    fn woodrow_wilson_cancels_uboot_roll() {
        let mut engine = EngineBuilder::new(22).with_resources(Empires, 2).build(); // die roll = 6

        engine.play_events(&ALL_EVENTS[15]);

        assert_eq!(0, engine.uboot_losses(1));
        assert_eq!(2, engine.state.resources_for(&Empires));
    }

    #[test]
    fn uboot_event_cancels_woodrow_wilson() {
        let mut engine = EngineBuilder::new(22).with_resources(Empires, 2).build(); // die roll = 6

        engine.play_events(&ALL_EVENTS[15]);
        engine.play_events(&ALL_EVENTS[25]);

        assert_eq!(4, engine.uboot_losses(1));
        assert_eq!(1, engine.state.resources_for(&Empires));
    }

    #[test]
    fn battle_of_verdun_requires_germany_to_attack_france_with_2_pr() {
        // not sure how to implement that...
    }
    #[test]
    fn battle_of_somme_requires_france_to_attack_germany_with_2_pr() {
        // not sure how to implement that...
    }

    #[test]
    fn brusilov_offensive_adds_1_per_die_to_russian_attack_on_austria() {
        let mut engine = EngineBuilder::new(30).build(); // die roll = 3, 6

        engine.play_events(&ALL_EVENTS[18]);
        engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: Russia,
            to: AustriaHungary,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![4, 6], dice);
    }

    #[test]
    fn brusilov_offensive_reduces_allies_pr_per_failed_die() {
        let mut engine = EngineBuilder::new(30).with_resources(Allies, 4).build(); // die roll = 3, 6
        let offensive = &Offensive {
            initiative: Allies,
            from: Russia,
            to: AustriaHungary,
            pr: 2,
        };

        engine.play_events(&ALL_EVENTS[18]);
        engine.compute_bonus(offensive);
        let dice = engine.roll_offensive_dice(2);
        engine.evaluate_attack_hits(0, 0, offensive, &dice);

        assert_eq!(3, engine.state.resources_for(&Allies));
    }

    #[test]
    fn romania_can_enter_war_with_event() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[19]);

        assert_eq!(
            NationState::AtWar(3),
            *engine.state.nations.get(&Romania).unwrap()
        );
    }

    #[test]
    fn mutinies_remove_one_die_from_french_attacks() {
        let mut engine = EngineBuilder::new(30).build(); // die roll = 3, 6

        engine.play_events(&ALL_EVENTS[21]);
        engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: France,
            to: Germany,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![3], dice);
    }

    #[test]
    fn battle_of_gaza_rethrows_succesful_dice_from_egypt_offensive() {
        let mut engine = EngineBuilder::new(45).build(); // die roll = 5, 2 ,3

        engine.play_events(&ALL_EVENTS[22]);
        engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: Egypt,
            to: OttomanEmpire,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);
        let hits = engine.evaluate_attack_hits(
            0,
            0,
            &Offensive {
                initiative: Allies,
                from: Egypt,
                to: OttomanEmpire,
                pr: 2,
            },
            &dice,
        );

        assert_eq!(0, hits);
    }

    #[test]
    fn lawrence_of_arabia_reduces_empires_pr_by_2() {
        let mut engine = EngineBuilder::new(30).with_resources(Empires, 3).build();

        engine.play_events(&ALL_EVENTS[23]);

        assert_eq!(1, engine.state.resources_for(&Empires));
    }

    #[test]
    fn battle_of_caporetto_adds_1_to_austria_attack_on_italy() {
        let mut engine = EngineBuilder::new(11).build();

        engine.play_events(&ALL_EVENTS[24]);
        let (_, att, _, _) = engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: AustriaHungary,
            to: Italy,
            pr: 2,
        });

        assert_eq!(1, att);
    }

    #[test]
    fn uboot_event_adds_1_to_naval_roll() {
        let mut engine = EngineBuilder::new(116).with_resources(Empires, 2).build(); // die roll = 5

        engine.play_events(&ALL_EVENTS[25]);

        assert_eq!(4, engine.uboot_losses(0));
    }

    #[test]
    fn flying_circus_increases_german_air_by_2() {
        let mut engine = EngineBuilder::new(30).build();

        engine.play_events(&ALL_EVENTS[26]);

        let (_, _, _, air) = engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: Germany,
            to: Russia,
            pr: 2,
        });

        assert_eq!(2, air);
    }

    #[test]
    fn flying_circus_lasts_two_turns() {
        let mut engine = EngineBuilder::new(30).build();

        engine.play_events(&ALL_EVENTS[26]);
        engine.new_turn();
        let (_, _, _, air) = engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: Germany,
            to: Russia,
            pr: 2,
        });
        assert_eq!(2, air);
        engine.new_turn();
        let (_, _, _, air) = engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: Germany,
            to: Russia,
            pr: 2,
        });

        assert_eq!(0, air);
    }

    #[test]
    fn greece_can_enter_war_with_event() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[27]);

        assert_eq!(
            NationState::AtWar(3),
            *engine.state.nations.get(&Greece).unwrap()
        );
    }

    #[test]
    fn zimmerman_telegram_makes_us_enter_war_given_lusitania_has_been_played() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[11]);
        engine.play_events(&ALL_EVENTS[28]);
        engine.new_turn();

        assert_eq!(Some(0), engine.state.lafayette);
    }

    #[test]
    fn lusitania_makes_us_enter_war_given_zimmerman_has_been_played() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[28]);
        engine.play_events(&ALL_EVENTS[11]);
        engine.new_turn();

        assert_eq!(Some(0), engine.state.lafayette);
    }

    #[test]
    fn once_us_enter_war_zimmerman_has_no_effect() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[28]);
        engine.play_events(&ALL_EVENTS[11]);
        engine.new_turn();
        engine.state.lafayette = Some(1);
        engine.new_turn();

        assert_eq!(Some(1), engine.state.lafayette);
    }
}

// 0 : 5 4 5
// 1 : 5 2 3
// 2 : 1 6 1
// 3 : 1 3 2
// 4 : 5 4 1
// 5 : 2 3 2
// 6 : 1 3 5
// 7 : 3 1 1
// 8 : 3 6 5
// 9 : 4 1 5
// 10 : 3 3 2
// 11 : 2 2 4
// 12 : 3 5 4
// 13 : 4 4 4
// 14 : 6 4 6
// 15 : 4 6 2
// 16 : 6 6 3
// 17 : 4 2 2
// 18 : 1 3 2
// 19 : 5 1 3
// 20 : 3 1 5
// 21 : 6 1 3
// 22 : 6 1 1
// 23 : 2 3 6
// 24 : 3 2 4
// 25 : 4 3 2
// 26 : 2 2 4
// 27 : 6 2 2
// 28 : 3 3 1
// 29 : 2 5 3
// 30 : 3 6 3
// 31 : 2 2 4
// 32 : 3 2 6
// 33 : 5 2 4
// 34 : 5 3 3
// 35 : 4 5 2
// 36 : 3 4 6
// 37 : 1 4 4
// 38 : 3 2 3
// 39 : 4 6 6
// 40 : 1 5 1
// 41 : 2 6 2
// 42 : 4 2 4
// 43 : 6 5 3
// 44 : 3 5 3
// 45 : 5 3 1
// 46 : 3 2 1
// 47 : 6 3 2
// 48 : 3 1 3
// 49 : 1 6 6
// 50 : 4 5 6
// 51 : 2 3 2
// 52 : 2 2 6
// 53 : 5 3 4
// 54 : 3 1 5
// 55 : 2 1 3
// 56 : 6 2 2
// 57 : 5 2 6
// 58 : 4 2 3
// 59 : 2 2 6
// 60 : 6 3 3
// 61 : 3 3 1
// 62 : 6 4 6
// 63 : 4 6 1
// 64 : 5 4 2
// 65 : 6 1 1
// 66 : 5 4 3
// 67 : 5 3 1
// 68 : 3 5 6
// 69 : 2 1 5
// 70 : 1 4 2
// 71 : 3 2 1
// 72 : 4 2 6
// 73 : 5 5 4
// 74 : 4 4 5
// 75 : 3 6 3
// 76 : 2 6 3
// 77 : 3 4 4
// 78 : 1 2 4
// 79 : 6 5 6
// 80 : 3 4 5
// 81 : 6 1 6
// 82 : 1 1 5
// 83 : 2 5 2
// 84 : 4 1 2
// 85 : 3 1 2
// 86 : 1 5 3
// 87 : 6 4 5
// 88 : 4 6 6
// 89 : 1 4 5
// 90 : 5 3 5
// 91 : 2 1 3
// 92 : 1 4 4
// 93 : 4 1 4
// 94 : 2 4 2
// 95 : 5 5 4
// 96 : 6 4 5
// 97 : 3 4 5
// 98 : 4 4 1
// 99 : 5 5 3
// 100 : 5 4 6
// 101 : 5 2 1
// 102 : 2 5 1
// 103 : 2 6 5
// 104 : 1 5 6
// 105 : 5 2 1
// 106 : 6 6 4
// 107 : 2 3 1
// 108 : 1 4 2
// 109 : 3 3 3
// 110 : 2 4 1
// 111 : 3 2 1
// 112 : 6 1 2
// 113 : 1 6 4
// 114 : 2 3 4
// 115 : 3 4 4
// 116 : 5 6 2
// 117 : 5 4 3
// 118 : 3 6 2
// 119 : 3 5 3
// 120 : 1 4 2
// 121 : 1 1 2
// 122 : 6 6 4
// 123 : 2 5 6
// 124 : 4 4 3
// 125 : 4 6 5
// 126 : 4 5 4
// 127 : 2 2 5
// 128 : 6 2 4
// 129 : 3 6 3
// 130 : 4 4 5
// 131 : 6 1 6
// 132 : 4 4 6
// 133 : 4 2 4
// 134 : 3 5 4
// 135 : 3 5 3
// 136 : 2 6 3
// 137 : 3 2 2
// 138 : 4 6 3
// 139 : 5 4 6
// 140 : 5 4 2
// 141 : 2 2 6
// 142 : 1 5 6
// 143 : 4 6 2
// 144 : 6 6 4
// 145 : 4 6 6
// 146 : 5 3 6
// 147 : 1 3 6
// 148 : 1 1 2
// 149 : 4 3 1
// 150 : 6 5 1
// 151 : 5 6 5
// 152 : 2 3 4
// 153 : 3 2 4
// 154 : 5 2 1
// 155 : 2 6 2
// 156 : 6 2 6
// 157 : 4 6 6
// 158 : 5 4 2
// 159 : 4 5 4
// 160 : 4 1 6
// 161 : 4 2 1
// 162 : 3 2 2
// 163 : 4 1 1
// 164 : 2 2 5
// 165 : 6 3 3
// 166 : 3 1 1
// 167 : 3 1 1
// 168 : 6 6 3
// 169 : 5 3 1
// 170 : 2 3 3
// 171 : 6 4 1
// 172 : 3 3 2
// 173 : 3 1 6
// 174 : 6 6 4
// 175 : 4 3 3
// 176 : 2 5 2
// 177 : 6 3 3
// 178 : 4 3 2
// 179 : 4 3 4
// 180 : 3 4 5
// 181 : 3 4 2
// 182 : 6 1 5
// 183 : 4 1 6
// 184 : 5 5 4
// 185 : 3 6 1
// 186 : 1 2 2
// 187 : 6 6 5
// 188 : 3 4 5
// 189 : 3 3 3
// 190 : 3 1 4
// 191 : 5 6 1
// 192 : 5 4 2
// 193 : 5 1 3
// 194 : 5 5 1
// 195 : 2 5 3
// 196 : 4 3 6
// 197 : 5 5 3
// 198 : 1 5 3
// 199 : 5 4 4
