use crate::logic::*;
use crate::side::*;
use crate::state::*;

impl GameLogic for RaceToTheSea {
    fn collect_resources(&mut self, state: &mut GameState) {
        self.previous.collect_resources(state)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        if self.active
            && (offensive.from == Nation::France && offensive.to == Nation::Germany
                || (offensive.from == Nation::Germany && offensive.to == Nation::France))
        {
            let (artillery_bonus, attack_bonus, defense_malus) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus + 1, defense_malus)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }

    fn roll_offensive_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_offensive_dice(state, num)
    }

    fn roll_artillery_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_artillery_dice(state, num)
    }

    fn evaluate_attack_hits(
        &self,
        state: &GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
    }

    fn evaluate_artillery_hits(
        &self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_artillery_hits(state, offensive, dice_roll)
    }

    fn reduce_pr(&self, state: &mut GameState, side: &Side, pr: u8) {
        self.previous.reduce_pr(state, side, pr)
    }

    fn apply_hits(&self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        self.previous.apply_hits(state, nation, hits)
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
    fn collect_resources(&mut self, state: &mut GameState) {
        self.previous.collect_resources(state)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        if self.active && offensive.from == Nation::France {
            let (artillery_bonus, attack_bonus, defense_malus) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus - 1, defense_malus)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }

    fn roll_offensive_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_offensive_dice(state, num)
    }

    fn roll_artillery_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_artillery_dice(state, num)
    }

    fn evaluate_attack_hits(
        &self,
        state: &GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
    }

    fn evaluate_artillery_hits(
        &self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_artillery_hits(state, offensive, dice_roll)
    }

    fn reduce_pr(&self, state: &mut GameState, side: &Side, pr: u8) {
        self.previous.reduce_pr(state, side, pr)
    }

    fn apply_hits(&self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        self.previous.apply_hits(state, nation, hits)
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
    fn collect_resources(&mut self, state: &mut GameState) {
        self.previous.collect_resources(state)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }

    fn roll_offensive_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_offensive_dice(state, num)
    }

    fn roll_artillery_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        let roll_artillery_dice = self.previous.roll_artillery_dice(state, num);
        if let Some(offensive) = &self.offensive {
            if offensive.from == Nation::Germany {
                roll_artillery_dice.into_iter().map(|die| die + 1).collect()
            } else {
                roll_artillery_dice
            }
        } else {
            roll_artillery_dice
        }
    }

    fn evaluate_attack_hits(
        &self,
        state: &GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
    }

    fn evaluate_artillery_hits(
        &self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_artillery_hits(state, offensive, dice_roll)
    }

    fn reduce_pr(&self, state: &mut GameState, side: &Side, pr: u8) {
        self.previous.reduce_pr(state, side, pr)
    }

    fn apply_hits(&self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        self.previous.apply_hits(state, nation, hits)
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
    fn collect_resources(&mut self, state: &mut GameState) {
        self.previous.collect_resources(state)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
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

    fn new_turn(&mut self, state: &mut GameState) {
        self.previous.new_turn(state);
    }

    fn roll_offensive_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        let added_die = if let Some(offensive) = &self.offensive {
            if offensive.from == Nation::GermanAfrica {
                1
            } else {
                0
            }
        } else {
            0
        };
        self.previous.roll_offensive_dice(state, num + added_die)
    }

    fn roll_artillery_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_artillery_dice(state, num)
    }

    fn evaluate_attack_hits(
        &self,
        state: &GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
    }

    fn evaluate_artillery_hits(
        &self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_artillery_hits(state, offensive, dice_roll)
    }

    fn reduce_pr(&self, state: &mut GameState, side: &Side, pr: u8) {
        self.previous.reduce_pr(state, side, pr)
    }

    fn apply_hits(&self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        self.previous.apply_hits(state, nation, hits)
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
    fn collect_resources(&mut self, state: &mut GameState) {
        self.previous.collect_resources(state);
        if self.active {
            let die = state.roll();
            self.reduce_pr(state, &Side::Allies, die);
        }
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        self.previous.compute_bonus(state, offensive)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.previous.new_turn(state);
        self.active = false;
    }

    fn roll_offensive_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_offensive_dice(state, num)
    }

    fn roll_artillery_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.previous.roll_artillery_dice(state, num)
    }

    fn evaluate_attack_hits(
        &self,
        state: &GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
    }

    fn evaluate_artillery_hits(
        &self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.previous
            .evaluate_artillery_hits(state, offensive, dice_roll)
    }

    fn reduce_pr(&self, state: &mut GameState, side: &Side, pr: u8) {
        self.previous.reduce_pr(state, side, pr)
    }

    fn apply_hits(&self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        self.previous.apply_hits(state, nation, hits)
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
        let (_, att, _) = engine.compute_bonus(&Offensive {
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

        // activate "Shells crisis"
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

        // activate "Von Lettow in Africa"
        engine.play_events(&ALL_EVENTS[7]);
        engine.collect_resources();

        assert_eq!(12, engine.state.resources_for(&Allies));
    }
}
