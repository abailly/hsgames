use crate::side::*;
use crate::state::*;

pub trait GameLogic {
    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8);
    fn roll_offensive_dice(&self, state: &mut GameState, num: u8) -> Vec<u8>;
    fn roll_artillery_dice(&self, state: &mut GameState, num: u8) -> Vec<u8>;
    fn evaluate_attack_hits(
        &self,
        state: &GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8;
    fn evaluate_artillery_hits(
        &self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8;
    fn reduce_pr(&self, state: &mut GameState, side: &Side, pr: u8);
    fn apply_hits(&self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult;
    fn new_turn(&mut self, state: &mut GameState);
}

impl<T: ?Sized + GameLogic> GameLogic for Box<T> {
    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        self.as_mut().compute_bonus(state, offensive)
    }

    fn roll_offensive_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.as_ref().roll_offensive_dice(state, num)
    }

    fn roll_artillery_dice(&self, state: &mut GameState, num: u8) -> Vec<u8> {
        self.as_ref().roll_artillery_dice(state, num)
    }

    fn evaluate_attack_hits(
        &self,
        state: &GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.as_ref()
            .evaluate_attack_hits(state, attack_bonus, defense_malus, offensive, dice_roll)
    }

    fn evaluate_artillery_hits(
        &self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: Vec<u8>,
    ) -> u8 {
        self.as_ref()
            .evaluate_artillery_hits(state, offensive, dice_roll)
    }

    fn reduce_pr(&self, state: &mut GameState, side: &Side, pr: u8) {
        self.as_ref().reduce_pr(state, side, pr)
    }

    fn apply_hits(&self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        self.as_ref().apply_hits(state, nation, hits)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.as_mut().new_turn(state)
    }
}

pub struct DummyLogic {}

impl DummyLogic {
    pub fn new() -> Self {
        DummyLogic {}
    }
}

impl GameLogic for DummyLogic {
    fn compute_bonus(&mut self, _state: &GameState, _offensive: &Offensive) -> (u8, i8, i8) {
        panic!("dummy logic")
    }
    fn roll_artillery_dice(&self, _state: &mut GameState, _num: u8) -> Vec<u8> {
        panic!("dummy logic")
    }

    fn roll_offensive_dice(&self, _state: &mut GameState, _num: u8) -> Vec<u8> {
        panic!("dummy logic")
    }

    fn evaluate_attack_hits(
        &self,
        _state: &GameState,
        _attack_bonus: i8,
        _defense_malus: i8,
        _offensive: &Offensive,
        _dice_roll: Vec<u8>,
    ) -> u8 {
        panic!("dummy logic")
    }

    fn evaluate_artillery_hits(
        &self,
        _state: &GameState,
        _offensive: &Offensive,
        _dice_roll: Vec<u8>,
    ) -> u8 {
        panic!("dummy logic")
    }

    fn reduce_pr(&self, _state: &mut GameState, _side: &Side, _pr: u8) {
        panic!("dummy logic")
    }

    fn apply_hits(&self, _state: &mut GameState, _nation: &Nation, _hits: u8) -> HitsResult {
        panic!("dummy logic")
    }

    fn new_turn(&mut self, _state: &mut GameState) {
        panic!("dummy logic")
    }
}
