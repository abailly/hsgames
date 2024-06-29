#![feature(assert_matches)]

use core::fmt;
use rand::prelude::*;
use std::io::{prelude::*, stdin, stdout, Stdin, Stdout};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::digit1;
use nom::combinator::{all_consuming, map, map_res};
use nom::IResult;

mod tech;
use tech::*;

mod side;
use side::*;

#[derive(Eq, PartialEq, Clone, Debug)]
enum Output {
    CurrentState(GameState),
    ChooseInitiative,
}

impl Display for Output {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Output::CurrentState(st) => write!(f, "Current state: {}", st),
            Output::ChooseInitiative => write!(f, "Select PR for initiative"),
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Input {
    Number(u8),
    Next,
}

trait Player {
    fn output(&mut self, message: &Output);
    fn input(&mut self) -> Input;
}

struct Console {
    inp: Stdin,
    outp: Stdout,
}

impl Player for Console {
    fn output(&mut self, message: &Output) {
        let mut stdout = self.outp.lock();
        stdout
            .write_all(format!("{}\n", message).as_bytes())
            .expect("Failed to write to stdout");
    }

    fn input(&mut self) -> Input {
        let mut command_string: String = String::new();
        self.inp.read_line(&mut command_string).unwrap();
        parse(command_string.trim()).unwrap_or_else(|_| self.input())
    }
}

struct RobotIO {}

impl Player for RobotIO {
    fn output(&mut self, _message: &Output) {
        // TODO
    }

    fn input(&mut self) -> Input {
        Input::Next
    }
}

fn num(input: &str) -> IResult<&str, Input> {
    map(map_res(digit1, |s: &str| s.parse::<u8>()), |n| {
        Input::Number(n)
    })(input)
}

#[derive(Debug, PartialEq, Eq)]
enum ParseError {
    TooManyCharacters(String),
    ParserFailed(String),
}

fn parse(string: &str) -> Result<Input, ParseError> {
    let next = map(
        alt((all_consuming(tag_no_case("n")), tag_no_case("next"))),
        |_| Input::Next,
    );
    let res = alt((next, num))(string);
    match res {
        Ok(("", res)) => Ok(res),
        Ok((rem, _)) => Err(ParseError::TooManyCharacters(rem.to_string())),
        Err(err) => Err(ParseError::ParserFailed(err.to_string())),
    }
}

enum PlayerType {
    Human,
    Robot,
}

struct Options {
    allies: PlayerType,
    empires: PlayerType,
}

const DEFAULT_OPTIONS: Options = Options {
    allies: PlayerType::Human,
    empires: PlayerType::Robot,
};

struct Players {
    allies_player: Box<dyn Player>,
    empires_player: Box<dyn Player>,
}

fn main() {
    let mut game_state = GameState::new(42);
    let mut players = initialise_players(DEFAULT_OPTIONS);
    while game_state.current_turn < 15 {
        run_turn(&mut players, &mut game_state);
    }
}

fn initialise_players(default_options: Options) -> Players {
    let allies_player = make_player(default_options.allies);
    let empires_player = make_player(default_options.empires);
    Players {
        allies_player,
        empires_player,
    }
}

fn make_player(player_type: PlayerType) -> Box<dyn Player> {
    match player_type {
        PlayerType::Human => Box::new(Console {
            inp: stdin(),
            outp: stdout(),
        }),
        PlayerType::Robot => Box::new(RobotIO {}),
    }
}

impl Player for Players {
    fn output(&mut self, message: &Output) {
        self.allies_player.output(message);
        self.empires_player.output(message);
    }

    fn input(&mut self) -> Input {
        todo!()
    }
}

fn run_turn(players: &mut Players, game_state: &mut GameState) {
    players.output(&Output::CurrentState(game_state.to_owned()));
    determine_initiative(players, game_state);

    let inp = players.allies_player.input();
    match inp {
        Input::Next => {
            game_state.current_turn += 1;
        }
        _ => {}
    }
}

const DEFAULT_INITIATIVE: [Side; 14] = [
    Side::Empires,
    Side::Empires,
    Side::Empires,
    Side::Allies,
    Side::Empires,
    Side::Allies,
    Side::Allies,
    Side::Allies,
    Side::Allies,
    Side::Allies,
    Side::Empires,
    Side::Empires,
    Side::Allies,
    Side::Allies,
];

/// Decide whose player has the initiative
/// This is only valid when turn > 1 as the empires automatically have the
/// initiative on the first turn
fn determine_initiative(players: &mut Players, game_state: &mut GameState) {
    if game_state.current_turn > 1 {
        players.output(&Output::ChooseInitiative);
        let allies_pr = match players.allies_player.input() {
            Input::Number(pr) => pr,
            _ => 0,
        };
        let empires_pr = match players.empires_player.input() {
            Input::Number(pr) => pr,
            _ => 0,
        };
        let allies_initiative = allies_pr + game_state.roll();
        let empires_initiative = empires_pr + game_state.roll();

        if allies_initiative > empires_initiative {
            game_state.initiative = Side::Allies;
        } else if allies_initiative < empires_initiative {
            game_state.initiative = Side::Empires;
        } else {
            game_state.initiative = DEFAULT_INITIATIVE[game_state.current_turn as usize - 1];
        }

        game_state.reduce_pr(Side::Allies, allies_pr);
        game_state.reduce_pr(Side::Empires, empires_pr);
    }
    players.output(&Output::CurrentState(game_state.to_owned()));
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct WarState {
    resources: u8,
    vp: u8,
    technologies: Box<Technologies>,
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct GameState {
    current_turn: u8,
    initiative: Side,
    russian_revolution: u8,
    breakdown: Box<HashMap<Nation, u8>>,
    state_of_war: Box<HashMap<Side, WarState>>,
    seed: u64,
    rng: StdRng,
}

impl GameState {
    fn new(seed: u64) -> Self {
        let breakdown = INITIAL_BREAKDOWN.iter().cloned().collect();
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
            russian_revolution: 0,
            breakdown: Box::new(breakdown),
            state_of_war: Box::new(initial_state_of_war),
            seed,
            rng: StdRng::seed_from_u64(seed),
        }
    }

    fn reduce_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        let st = self.state_of_war.get_mut(&side).unwrap();
        if st.resources >= pr {
            st.resources -= pr;
        }
        self
    }

    fn increase_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        let st = self.state_of_war.get_mut(&side).unwrap();
        st.resources += pr;
        self
    }

    fn roll(&mut self) -> u8 {
        self.rng.gen_range(1..=6)
    }
}

impl Display for GameState {
    /// TODO: take care of writeln! result
    #[allow(unused_must_use)]
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "Turn: {}", self.current_turn);
        writeln!(f, "Russian Revolution: {}", self.russian_revolution);
        writeln!(f, "Breakdown:");
        for (nation, breakdown) in self.breakdown.iter() {
            writeln!(f, "\t{}: {}", nation, breakdown);
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
mod fixtures {
    use crate::{GameState, Output, Player, Side::*};
    use crate::{Input, Players, Side};

    pub struct PlayerDouble {
        pub out: Box<Vec<Output>>,
        pub inp: Box<Vec<Input>>,
    }

    impl Player for PlayerDouble {
        fn output(&mut self, message: &Output) {
            self.out.push(message.clone());
        }

        fn input(&mut self) -> Input {
            self.inp.pop().unwrap()
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
                Allies => self.allies_input.push(input),
                Empires => self.empires_input.push(input),
            }
            self
        }

        pub fn build(&self) -> Players {
            let allies = PlayerDouble {
                out: Box::new(Vec::new()),
                inp: Box::new(self.allies_input.clone()),
            };
            let empires = PlayerDouble {
                out: Box::new(Vec::new()),
                inp: Box::new(self.empires_input.clone()),
            };
            Players {
                allies_player: Box::new(allies),
                empires_player: Box::new(empires),
            }
        }
    }

    pub struct StateBuilder {
        state: GameState,
    }

    impl StateBuilder {
        pub fn new(seed: i32) -> Self {
            StateBuilder {
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

        pub fn build(&self) -> GameState {
            self.state.clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::{
        determine_initiative,
        fixtures::{PlayersBuilder, StateBuilder},
        parse, GameState,
        Input::*,
        Side::*,
    };

    #[test]
    fn parses_next_command() {
        for command in &["next", "n", "N", "Next"] {
            assert_eq!(parse(command), Ok(Next));
        }
    }

    #[test]
    fn parses_number() {
        assert_eq!(parse("12"), Ok(Number(12)));
    }

    #[test]
    fn rejects_invalid_commands() {
        for command in &["ne", "x", ""] {
            assert_matches!(parse(command), Err(_));
        }
    }

    #[test]
    fn adjusts_resources_given_a_side_and_some_amount() {
        let mut state = GameState::new(12);
        state.increase_pr(Allies, 4);
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
        state.reduce_pr(Allies, 3);
        assert_eq!(1, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn cannot_reduce_resources_below_0() {
        let mut state = GameState::new(12);
        state.reduce_pr(Allies, 3);
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn empires_has_initiative_on_first_turn() {
        let mut state = StateBuilder::new(12).build();
        let mut players = PlayersBuilder::new().build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Empires, state.initiative)
    }

    #[test]
    fn allies_have_initiative_on_second_turn_given_they_bid_more_pr() {
        let mut state = StateBuilder::new(12).on_turn(2).build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(1))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Allies, state.initiative)
    }

    #[test]
    fn empires_have_initiative_on_second_turn_given_they_bid_more_pr() {
        let mut state = StateBuilder::new(12).on_turn(2).build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(1))
            .with_input(Empires, Number(2))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Empires, state.initiative)
    }

    #[test]
    fn initiative_consumes_bid_pr_from_both_sides() {
        let allies_bid = 1;
        let empires_bid = 2;
        let initial_allies_resources = 4;
        let initial_empires_resources = 5;

        let mut state = StateBuilder::new(12)
            .with_resources(Allies, initial_allies_resources)
            .with_resources(Empires, initial_empires_resources)
            .on_turn(2)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(allies_bid))
            .with_input(Empires, Number(empires_bid))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(
            initial_allies_resources - allies_bid,
            state.state_of_war.get(&Allies).unwrap().resources
        );
        assert_eq!(
            initial_empires_resources - empires_bid,
            state.state_of_war.get(&Empires).unwrap().resources
        )
    }

    #[test]
    fn allies_have_initiative_if_die_roll_is_better_given_equal_bid() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(2)
            .build();

        let allies_bid = 2;
        let empires_bid = 2;

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(allies_bid))
            .with_input(Empires, Number(empires_bid))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Allies, state.initiative)
    }

    #[test]
    fn empires_have_initiative_on_turn_2_given_equal_bid_and_die() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(2)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(4))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Empires, state.initiative)
    }

    #[test]
    fn allies_have_initiative_on_turn_4_given_equal_bid_and_die() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(4)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(4))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Allies, state.initiative)
    }
}
