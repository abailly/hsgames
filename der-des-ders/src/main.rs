#![feature(assert_matches)]

use core::fmt;
use nom::sequence::{separated_pair, tuple};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::io::{prelude::*, stdin, stdout, Stdin, Stdout};

use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, map, map_res};
use nom::{IResult, Parser};

mod tech;
use tech::*;

mod side;
use side::*;

mod state;
use state::*;

#[derive(Eq, PartialEq, Clone, Debug)]
enum Output {
    CurrentState(GameState),
    ChooseInitiative,
    ImproveTechnologies,
    LaunchOffensive,
    WrongInput(Input),
    NotEnoughResources(u8, u8),
    CountryAlreadyAttacked(Nation),
    AttackingNonAdjacentCountry(Nation, Nation),
    OperationalLevelTooLow(u8, u8),
}

impl Display for Output {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Output::CurrentState(st) => write!(f, "Current state: {}", st),
            Output::ChooseInitiative => write!(f, "Select PR for initiative"),
            Output::ImproveTechnologies => write!(f, "Select PR to improve technologies, or Pass"),
            Output::LaunchOffensive => write!(f, "Spend PR to launch offensive, or Pass"),
            Output::WrongInput(inp) => write!(f, "Invalid input: {:?}", inp),
            Output::NotEnoughResources(wanted, actual) => {
                write!(f, "Not enough resources ({}) to spend {}", actual, wanted)
            }
            Output::CountryAlreadyAttacked(country) => {
                write!(f, "Country already attacked: {}", country)
            }
            Output::AttackingNonAdjacentCountry(from, to) => {
                write!(f, "{} is not adjacent to  {}", from, to)
            }
            Output::OperationalLevelTooLow(maximum, actual) => {
                write!(f, "Operational level ({}) too low for {}", maximum, actual)
            }
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Input {
    Number(u8),
    Pass,
    Select(TechnologyType, u8),
    Offensive(Nation, Nation, u8),
    Next,
}

trait Player {
    fn output(&mut self, message: &Output);
    fn input(&mut self) -> Input;
    fn out(&self) -> Vec<Output>;
}

struct Console {
    side: Side,
    inp: Stdin,
    outp: Stdout,
    out: Vec<Output>,
}

impl Player for Console {
    fn output(&mut self, message: &Output) {
        self.out.push(message.clone());
        let mut stdout = self.outp.lock();
        stdout
            .write_all(format!("{}: {}\n", self.side, message).as_bytes())
            .expect("Failed to write to stdout");
    }

    fn input(&mut self) -> Input {
        let mut command_string: String = String::new();
        self.inp.read_line(&mut command_string).unwrap();
        parse(command_string.trim()).unwrap_or_else(|_| self.input())
    }

    fn out(&self) -> Vec<Output> {
        self.out.clone()
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

    fn out(&self) -> Vec<Output> {
        vec![]
    }
}

fn num(input: &str) -> IResult<&str, Input> {
    map(map_res(digit1, |s: &str| s.parse::<u8>()), |n| {
        Input::Number(n)
    })(input)
}

fn country(input: &str) -> IResult<&str, Nation> {
    alt((
        tag_no_case("france").map(|_| Nation::France),
        tag_no_case("germany").map(|_| Nation::Germany),
        tag_no_case("italy").map(|_| Nation::Italy),
        tag_no_case("austria").map(|_| Nation::AustriaHungary),
        tag_no_case("russia").map(|_| Nation::Russia),
        tag_no_case("ottoman").map(|_| Nation::OttomanEmpire),
        tag_no_case("bulgaria").map(|_| Nation::Bulgaria),
    ))(input)
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
    let pass = map(
        alt((all_consuming(tag_no_case("p")), tag_no_case("pass"))),
        |_| Input::Pass,
    );
    let select = map(
        all_consuming(separated_pair(
            alt((
                tag_no_case("attack").map(|_| TechnologyType::Attack),
                tag_no_case("defense").map(|_| TechnologyType::Defense),
                tag_no_case("artillery").map(|_| TechnologyType::Artillery),
                tag_no_case("air").map(|_| TechnologyType::Air),
            )),
            char(' '),
            num,
        )),
        |(tech, inp)| match inp {
            Input::Number(n) => Input::Select(tech, n),
            _ => panic!("Invalid input"), // never reached
        },
    );
    let offensive = map(
        all_consuming(tuple((
            alt((
                tag_no_case("offensive").map(|_| ()),
                tag_no_case("off").map(|_| ()),
            )),
            char(' '),
            country,
            char(' '),
            country,
            char(' '),
            num,
        ))),
        |(_, _, attacker, _, defender, _, inp)| {
            match inp {
                Input::Number(n) => Input::Offensive(attacker, defender, n),
                _ => panic!("Invalid input"), // never reached
            }
        },
    );

    let res = alt((next, pass, select, offensive, num))(string);
    match res {
        Ok(("", res)) => Ok(res),
        Ok((rem, _)) => Err(ParseError::TooManyCharacters(rem.to_string())),
        Err(err) => Err(ParseError::ParserFailed(err.to_string())),
    }
}

enum PlayerType {
    Human,
    #[allow(dead_code)]
    Robot,
}

struct Options {
    allies: PlayerType,
    empires: PlayerType,
}

const DEFAULT_OPTIONS: Options = Options {
    allies: PlayerType::Human,
    empires: PlayerType::Human,
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
    let allies_player = make_player(Side::Allies, default_options.allies);
    let empires_player = make_player(Side::Empires, default_options.empires);
    Players {
        allies_player,
        empires_player,
    }
}

fn make_player(side: Side, player_type: PlayerType) -> Box<dyn Player> {
    match player_type {
        PlayerType::Human => Box::new(Console {
            side,
            inp: stdin(),
            outp: stdout(),
            out: vec![],
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

    fn out(&self) -> Vec<Output> {
        self.allies_player
            .out()
            .iter()
            .chain(self.empires_player.out().iter())
            .cloned()
            .collect()
    }
}

fn run_turn(players: &mut Players, game_state: &mut GameState) {
    players.output(&Output::CurrentState(game_state.to_owned()));
    determine_initiative(players, game_state);
    collect_resources(game_state);

    players.output(&Output::CurrentState(game_state.to_owned()));
    run_player_turn(game_state.initiative, players, game_state);
    run_player_turn(game_state.initiative.other(), players, game_state);

    let inp = players.allies_player.input();
    if let Input::Next = inp {
        game_state.current_turn += 1;
    }
}

fn run_player_turn(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    improve_technologies(initiative, players, game_state);
    launch_offensives(initiative, players, game_state);
}

fn improve_technologies(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    let mut improved: Vec<TechnologyType> = vec![];

    while improved.len() < 4 {
        player.output(&Output::ImproveTechnologies);
        match player.input() {
            Input::Select(tech, n) => {
                if improved.contains(&tech) {
                    continue;
                }
                let die = game_state.roll();
                improve_technology(game_state, initiative, tech, n, die);
                improved.push(tech);
            }
            Input::Pass => break,
            other => player.output(&Output::WrongInput(other)),
        }
    }
}

fn improve_technology(
    game_state: &mut GameState,
    initiative: Side,
    tech: TechnologyType,
    n: u8,
    die: u8,
) {
    let year = game_state.current_year();
    let techs = &mut game_state
        .state_of_war
        .get_mut(&initiative)
        .unwrap()
        .technologies;
    let technologies = match initiative {
        Side::Allies => &ALLIES_TECHNOLOGIES,
        Side::Empires => &EMPIRE_TECHNOLOGIES,
    };
    match tech {
        TechnologyType::Attack => {
            let current = techs.attack;
            if let Some(technology) = &technologies[0][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.attack += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
        TechnologyType::Defense => {
            let current = techs.defense;
            if let Some(technology) = &technologies[1][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.defense += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
        TechnologyType::Artillery => {
            let current = techs.artillery;
            if let Some(technology) = &technologies[2][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.artillery += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
        TechnologyType::Air => {
            let current = techs.air;
            if let Some(technology) = &technologies[3][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.air += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
    }
}

fn launch_offensives(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    let mut nations = game_state.all_nations_at_war(initiative);

    while !nations.is_empty() {
        player.output(&Output::LaunchOffensive);
        match player.input() {
            Input::Offensive(from, _, _) if !nations.contains(&from) => {
                player.output(&Output::CountryAlreadyAttacked(from));
            }
            Input::Offensive(from, to, _) if !from.adjacent_to(&to) => {
                player.output(&Output::AttackingNonAdjacentCountry(from, to));
            }
            Input::Offensive(from, to, pr) => {
                let operational = game_state.nations.get(&from).unwrap().operational_level();
                let resources = game_state.state_of_war.get(&initiative).unwrap().resources;
                if operational < pr {
                    player.output(&Output::OperationalLevelTooLow(operational, pr));
                } else if resources < pr {
                    player.output(&Output::NotEnoughResources(pr, resources));
                } else {
                    resolve_offensive(game_state, initiative, from, to, pr);
                    game_state.reduce_pr(initiative, pr);
                    nations.retain(|&nat| nat != from);
                }
            }
            Input::Pass => return,
            _ => todo!(),
        }
    }
}

fn resolve_offensive(
    game_state: &mut GameState,
    initiative: Side,
    from: Nation,
    to: Nation,
    pr: u8,
) {
    let artillery_bonus = game_state.artillery_bonus(&initiative);

    let attack_bonus = game_state.attack_bonus(&initiative);

    let defense_malus = game_state.defense_bonus(&initiative.other());

    let dice: Vec<u8> = (0..pr).map(|_| game_state.roll()).collect();
    let artillery_dice: Vec<u8> = (0..artillery_bonus).map(|_| game_state.roll()).collect();

    let attack_country = |die: u8| {
        return die + attack_bonus - defense_malus
            >= game_state.countries.get(&from).unwrap().attack_factor;
    };

    let bomb_country =
        |die: u8| return die >= game_state.countries.get(&from).unwrap().attack_factor;

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

    if let NationState::AtWar(breakdown) = game_state.nations.get_mut(&to).unwrap() {
        *breakdown -= attack_hits + artillery_hits;
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
///
/// * On turn 1, the empires automatically have the initiative
/// * On subsequent turns, players bid PR for initiative and add a die roll. The player with the highest
///   total has the initiative. In case of a tie, the initiative is defined from the DEFAULT_INITIATIVE
///   array.
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

        game_state.initiative = match allies_initiative.cmp(&empires_initiative) {
            Ordering::Greater => Side::Allies,
            Ordering::Less => Side::Empires,
            Ordering::Equal => DEFAULT_INITIATIVE[game_state.current_turn as usize - 1],
        };

        game_state.reduce_pr(Side::Allies, allies_pr);
        game_state.reduce_pr(Side::Empires, empires_pr);
    }
    players.output(&Output::CurrentState(game_state.to_owned()));
}

fn collect_resources(game_state: &mut GameState) {
    game_state.increase_pr(Side::Allies, tally_resources(game_state, &Side::Allies));
    game_state.increase_pr(Side::Empires, tally_resources(game_state, &Side::Empires));
}

fn tally_resources(game_state: &GameState, pr_for_side: &Side) -> u8 {
    game_state
        .nations
        .iter()
        .fold(0, |acc, (nation, status)| match status {
            NationState::AtWar(breakdown) => match game_state.countries.get(nation) {
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

#[cfg(test)]
mod fixtures {
    use crate::{GameState, Nation, NationState, Output, Player, Side::*};
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
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use crate::{
        collect_resources, determine_initiative,
        fixtures::{PlayersBuilder, StateBuilder},
        parse, GameState,
        Input::*,
        Nation::*,
        NationState::*,
        Side::*,
        TechnologyType::*,
    };

    #[test]
    fn parses_next_command() {
        for command in &["next", "n", "N", "Next"] {
            assert_eq!(parse(command), Ok(Next));
        }
    }

    #[test]
    fn parses_pass_command() {
        for command in &["pass", "p", "P", "Pass"] {
            assert_eq!(parse(command), Ok(Pass));
        }
    }

    #[test]
    fn parses_select_command() {
        assert_eq!(parse("attack 2"), Ok(Select(Attack, 2)));
        assert_eq!(parse("defense 3"), Ok(Select(Defense, 3)));
        assert_eq!(parse("artillery 1"), Ok(Select(Artillery, 1)));
        assert_eq!(parse("air 4"), Ok(Select(Air, 4)));
        assert_matches!(parse("attack foo"), Err(_));
    }

    #[test]
    fn parses_offensive_command() {
        assert_eq!(
            parse("offensive France Germany 2"),
            Ok(Offensive(France, Germany, 2))
        );
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

    #[test]
    fn collect_resources_increase_pr_for_each_side() {
        let mut state = StateBuilder::new(14).build();

        collect_resources(&mut state);

        assert_eq!(14, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(9, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_cannot_increase_pr_over_20() {
        let mut state = StateBuilder::new(14).build();

        collect_resources(&mut state);
        collect_resources(&mut state);

        assert_eq!(20, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(18, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_changes_allies_pr_when_italy_goes_at_war() {
        let mut state = StateBuilder::new(14).with_nation(Italy, AtWar(5)).build();

        collect_resources(&mut state);

        assert_eq!(16, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(9, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_changes_allies_pr_when_russia_breakdown_changes() {
        let mut state = StateBuilder::new(14).with_nation(Russia, AtWar(3)).build();

        collect_resources(&mut state);

        assert_eq!(10, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(9, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_changes_empires_pr_when_bulgaria_goes_at_war() {
        let mut state = StateBuilder::new(14)
            .with_nation(Bulgaria, AtWar(3))
            .build();

        collect_resources(&mut state);

        assert_eq!(14, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(10, state.state_of_war.get(&Empires).unwrap().resources);
    }
}

#[cfg(test)]
mod technologies {

    use crate::{
        fixtures::{PlayersBuilder, StateBuilder},
        improve_technologies,
        Input::*,
        Output,
        Side::*,
        Technologies,
        TechnologyType::*,
        ZERO_TECHNOLOGIES,
    };

    #[test]
    fn technology_does_not_change_given_player_passes_on_it() {
        let mut state = StateBuilder::new(14).with_initiative(Empires).build();
        let mut players = PlayersBuilder::new().with_input(Empires, Pass).build();

        improve_technologies(Empires, &mut players, &mut state);

        assert_eq!(
            ZERO_TECHNOLOGIES,
            *state.state_of_war.get(&Empires).unwrap().technologies
        );
    }

    #[test]
    fn empires_improve_attack_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
            .with_resources(Empires, 4)
            .with_initiative(Empires)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Select(Attack, 2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Empires).unwrap().technologies
        );
        assert_eq!(2, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn empires_cannot_improve_attack_technology_too_soon() {
        let mut state = StateBuilder::new(14)
            .with_resources(Empires, 4)
            .with_initiative(Empires)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Select(Attack, 2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 0,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Empires).unwrap().technologies
        );
        assert_eq!(4, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn allies_improve_defense_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_technologies(
                Allies,
                Technologies {
                    defense: 1,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                defense: 2,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_cannot_improve_artillery_technology_1_level_before_year_available() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Artillery, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            ZERO_TECHNOLOGIES,
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_improve_attack_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 3,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_initiative(Allies)
            .on_turn(11)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Attack, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 4,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_can_improve_technologies_1_level_until_pass() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Attack, 2))
            .with_input(Allies, Select(Artillery, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 1,
                artillery: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_cannot_improve_defense_technology_level_past_3() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    defense: 3,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                defense: 3,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
    }

    #[test]
    fn cannot_improve_same_technology_twice() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                defense: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
    }

    #[test]
    fn allies_improve_air_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(3)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Air, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                air: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn message_player_given_input_is_inappropriate_for_improve_tech_phase() {
        let mut state = StateBuilder::new(14).with_resources(Empires, 4).build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Number(2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut state);

        assert_eq!(
            vec![
                Output::ImproveTechnologies,
                Output::WrongInput(Number(2)),
                Output::ImproveTechnologies
            ],
            players.empires_player.out()
        );
    }
}

#[cfg(test)]
mod offensives {

    use crate::{
        fixtures::{PlayersBuilder, StateBuilder},
        launch_offensives,
        Input::*,
        Nation::*,
        NationState::*,
        Output,
        Side::*,
        Technologies, ZERO_TECHNOLOGIES,
    };

    #[test]
    fn initiative_player_can_spend_pr_to_launch_offensive_between_adjacent_countries() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
        assert_eq!(3, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn player_cannot_spend_more_pr_than_available_for_offensives() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 2)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 3))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::NotEnoughResources(3, 2),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(2, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn initiative_player_launch_several_offensives() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Offensive(Russia, OttomanEmpire, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
        assert_eq!(AtWar(4), *state.nations.get(&OttomanEmpire).unwrap());
        assert_eq!(2, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn cannot_launch_offensive_to_not_adjacent_country() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::AttackingNonAdjacentCountry(France, AustriaHungary),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn initiative_player_launch_cannot_launch_several_offensives_from_same_country() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Russia, Germany, 1))
            .with_input(Allies, Offensive(Russia, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::LaunchOffensive,
                Output::CountryAlreadyAttacked(Russia),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(3, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn pr_for_offensive_cannot_exceed_operational_level() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Serbia, AustriaHungary, 2))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::OperationalLevelTooLow(1, 2),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn attack_technology_provides_offensive_bonus() {
        let mut state = StateBuilder::new(18)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn defense_technology_provides_offensive_malus() {
        let mut state = StateBuilder::new(18)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_technologies(
                Empires,
                Technologies {
                    defense: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(8), *state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn offensive_launch_as_many_dice_as_pr_expended() {
        let mut state = StateBuilder::new(15)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 3))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(5), *state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn artillery_technology_adds_more_dice_to_throw() {
        let mut state = StateBuilder::new(15)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    artillery: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(5), *state.nations.get(&Germany).unwrap());
    }
}
