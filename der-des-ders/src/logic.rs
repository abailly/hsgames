use crate::side::*;
use crate::state::*;
use crate::TechEffects;

pub trait GameLogic {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        None
    }
    fn collect_resources(&mut self, state: &mut GameState) {
        if let Some(previous) = self.previous() {
            previous.collect_resources(state);
        }
    }
    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        if let Some(previous) = self.previous() {
            previous.compute_bonus(state, offensive)
        } else {
            (0, 0, 0, 0)
        }
    }
    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        if let Some(previous) = self.previous() {
            previous.roll_offensive_dice(state, num)
        } else {
            vec![]
        }
    }
    fn roll_artillery_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        if let Some(previous) = self.previous() {
            previous.roll_artillery_dice(state, num)
        } else {
            vec![]
        }
    }
    fn evaluate_attack_hits(
        &mut self,
        state: &mut GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        if let Some(previous) = self.previous() {
            previous.evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
        } else {
            0
        }
    }
    fn evaluate_artillery_hits(
        &mut self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        if let Some(previous) = self.previous() {
            previous.evaluate_artillery_hits(state, offensive, dice_roll)
        } else {
            0
        }
    }
    fn reduce_pr(&mut self, state: &mut GameState, side: &Side, pr: u8) {
        if let Some(previous) = self.previous() {
            previous.reduce_pr(state, side, pr)
        }
    }
    fn apply_hits(&mut self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        if let Some(previous) = self.previous() {
            previous.apply_hits(state, nation, hits)
        } else {
            HitsResult::NoResult
        }
    }
    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> (u8, u8) {
        if let Some(previous) = self.previous() {
            previous.uboot_losses(state, bonus)
        } else {
            (0, 0)
        }
    }
    fn blockade_effect(&mut self, state: &mut GameState, bonus: u8) -> (u8, u8) {
        if let Some(previous) = self.previous() {
            previous.blockade_effect(state, bonus)
        } else {
            (0, 0)
        }
    }
    fn new_turn(&mut self, state: &mut GameState) {
        if let Some(previous) = self.previous() {
            previous.new_turn(state)
        }
    }
    fn event_activated(&mut self, event: &ActiveEvent) {
        if let Some(previous) = self.previous() {
            previous.event_activated(event)
        }
    }
}

impl<T: ?Sized + GameLogic> GameLogic for Box<T> {
    fn collect_resources(&mut self, state: &mut GameState) {
        self.as_mut().collect_resources(state)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> TechEffects {
        self.as_mut().compute_bonus(state, offensive)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.as_mut().roll_offensive_dice(state, num)
    }

    fn roll_artillery_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.as_mut().roll_artillery_dice(state, num)
    }

    fn evaluate_attack_hits(
        &mut self,
        state: &mut GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        self.as_mut()
            .evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
    }

    fn evaluate_artillery_hits(
        &mut self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        self.as_mut()
            .evaluate_artillery_hits(state, offensive, dice_roll)
    }

    fn reduce_pr(&mut self, state: &mut GameState, side: &Side, pr: u8) {
        self.as_mut().reduce_pr(state, side, pr)
    }

    fn apply_hits(&mut self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        self.as_mut().apply_hits(state, nation, hits)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.as_mut().new_turn(state)
    }

    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> (u8, u8) {
        self.as_mut().uboot_losses(state, bonus)
    }

    fn blockade_effect(&mut self, state: &mut GameState, bonus: u8) -> (u8, u8) {
        self.as_mut().blockade_effect(state, bonus)
    }

    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        None
    }

    fn event_activated(&mut self, event: &ActiveEvent) {
        self.as_mut().event_activated(event)
    }
}

pub struct DummyLogic {}

impl DummyLogic {
    pub fn new() -> Self {
        DummyLogic {}
    }
}

impl GameLogic for DummyLogic {
    fn collect_resources(&mut self, _state: &mut GameState) {
        panic!("dummy logic")
    }

    fn compute_bonus(&mut self, _state: &GameState, _offensive: &Offensive) -> TechEffects {
        panic!("dummy logic")
    }
    fn roll_artillery_dice(&mut self, _state: &mut GameState, _num: u8) -> Vec<u8> {
        panic!("dummy logic")
    }

    fn roll_offensive_dice(&mut self, _state: &mut GameState, _num: u8) -> Vec<u8> {
        panic!("dummy logic")
    }

    fn evaluate_attack_hits(
        &mut self,
        _state: &mut GameState,
        _attack_bonus: i8,
        _defense_malus: i8,
        _offensive: &Offensive,
        _dice_roll: &Vec<u8>,
    ) -> u8 {
        panic!("dummy logic")
    }

    fn evaluate_artillery_hits(
        &mut self,
        _state: &GameState,
        _offensive: &Offensive,
        _dice_roll: &Vec<u8>,
    ) -> u8 {
        panic!("dummy logic")
    }

    fn reduce_pr(&mut self, _state: &mut GameState, _side: &Side, _pr: u8) {
        panic!("dummy logic")
    }

    fn apply_hits(&mut self, _state: &mut GameState, _nation: &Nation, _hits: u8) -> HitsResult {
        panic!("dummy logic")
    }

    fn new_turn(&mut self, _state: &mut GameState) {
        panic!("dummy logic")
    }

    fn uboot_losses(&mut self, _state: &mut GameState, _bonus: u8) -> (u8, u8) {
        panic!("dummy logic")
    }

    fn blockade_effect(&mut self, _state: &mut GameState, _bonus: u8) -> (u8, u8) {
        panic!("dummy logic")
    }
    fn event_activated(&mut self, _event: &ActiveEvent) {
        panic!("dummy logic")
    }
}
