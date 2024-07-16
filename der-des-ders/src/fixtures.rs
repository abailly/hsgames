use crate::{GameEngine, GameState, Nation, NationState, Output, Player, Side::*};
use crate::{Input, Players, Side};

pub struct PlayerDouble {
    pub out: Vec<Output>,
    pub inp: Vec<Input>,
}

impl Player for PlayerDouble {
    fn output(&mut self, message: &Output) {
        self.out.push(message.clone());
    }

    fn input(&mut self) -> Input {
        self.inp.pop().unwrap()
    }

    fn out(&self) -> Vec<Output> {
        self.out.clone()
    }
}

pub struct PlayersBuilder {
    allies_input: Vec<Input>,
    empires_input: Vec<Input>,
}

impl PlayersBuilder {
    pub fn new() -> Self {
        Self {
            allies_input: Vec::new(),
            empires_input: Vec::new(),
        }
    }

    pub fn with_input(&mut self, side: Side, input: Input) -> &mut Self {
        match side {
            Allies => self.allies_input.insert(0, input),
            Empires => self.empires_input.insert(0, input),
        }
        self
    }

    pub fn build(&self) -> Players {
        let allies = PlayerDouble {
            out: Vec::new(),
            inp: self.allies_input.clone(),
        };
        let empires = PlayerDouble {
            out: Vec::new(),
            inp: self.empires_input.clone(),
        };
        Players {
            allies_player: Box::new(allies),
            empires_player: Box::new(empires),
        }
    }
}

pub struct EngineBuilder {
    state: GameState,
}

impl EngineBuilder {
    pub fn new(seed: i32) -> Self {
        EngineBuilder {
            state: GameState::new(seed as u64),
        }
    }

    pub fn with_resources(&mut self, side: Side, pr: u8) -> &mut Self {
        self.state.increase_pr(side, pr);
        self
    }

    pub fn on_turn(&mut self, turn: u8) -> &mut Self {
        self.state.current_turn = turn;
        self
    }

    pub fn build(&self) -> GameEngine {
        GameEngine::with_state(self.state.to_owned())
    }

    pub(crate) fn with_nation(&mut self, nation: Nation, status: NationState) -> &mut Self {
        self.state.nations.insert(nation, status);
        self
    }

    pub(crate) fn with_initiative(&mut self, initiative: Side) -> &mut Self {
        self.state.initiative = initiative;
        self
    }

    pub(crate) fn with_technologies(
        &mut self,
        side: Side,
        technologies: crate::Technologies,
    ) -> &mut Self {
        self.state.state_of_war.get_mut(&side).unwrap().technologies = Box::new(technologies);
        self
    }
}
