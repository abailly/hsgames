use core::fmt;
use nom::sequence::{separated_pair, tuple};
use std::fmt::{Display, Formatter};
use std::io::{prelude::*, Stdin, Stdout};

use nom::branch::alt;
use nom::bytes::complete::tag_no_case;
use nom::character::complete::{char, digit1};
use nom::combinator::{all_consuming, map, map_res};
use nom::{IResult, Parser};

use crate::side::*;
use crate::{tech::*, GameState};

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Output {
    CurrentState(GameState),
    ChooseInitiative,
    ImproveTechnologies,
    LaunchOffensive,
    WrongInput(Input),
    NotEnoughResources(u8, u8),
    CountryAlreadyAttacked(Nation),
    AttackingNonAdjacentCountry(Nation, Nation),
    OperationalLevelTooLow(u8, u8),
    OffensiveResult {
        from: Nation,
        to: Nation,
        attack_hits: u8,
        artillery_hits: u8,
    },
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
            Output::OffensiveResult {
                from,
                to,
                attack_hits,
                artillery_hits,
            } => write!(
                f,
                "Offensive from {} to {}: {} hits",
                from,
                to,
                attack_hits + artillery_hits
            ),
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum Input {
    Number(u8),
    Pass,
    Select(TechnologyType, u8),
    Offensive(Nation, Nation, u8),
    Next,
}

pub trait Player {
    fn output(&mut self, message: &Output);
    fn input(&mut self) -> Input;
    fn out(&self) -> Vec<Output>;
}

pub struct Console {
    pub side: Side,
    pub inp: Stdin,
    pub outp: Stdout,
    pub out: Vec<Output>,
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

pub struct RobotIO {}

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
        tag_no_case("serbia").map(|_| Nation::Serbia),
        tag_no_case("ottoman").map(|_| Nation::OttomanEmpire),
        tag_no_case("bulgaria").map(|_| Nation::Bulgaria),
        tag_no_case("egypt").map(|_| Nation::Egypt),
        tag_no_case("romania").map(|_| Nation::Romania),
        tag_no_case("greece").map(|_| Nation::Greece),
        tag_no_case("aef").map(|_| Nation::FrenchAfrica),
        tag_no_case("germanafrica").map(|_| Nation::GermanAfrica),
    ))(input)
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    TooManyCharacters(String),
    ParserFailed(String),
}

pub fn parse(string: &str) -> Result<Input, ParseError> {
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

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::{parse, Input::*, Nation::*, TechnologyType::*};

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
}
