use core::fmt;
use std::io::{prelude::*, stdin, stdout, Stdin, Stdout};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

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
    fn output(&self, message: &Output);
    fn input(&self) -> Input;
}

struct Console {
    inp: Stdin,
    outp: Stdout,
}

impl Player for Console {
    fn output(&self, message: &Output) {
        let mut stdout = self.outp.lock();
        stdout.write_all(format!("{}\n", message).as_bytes());
    }

    fn input(&self) -> Input {
        let mut command_string: String = String::new();
        self.inp.read_line(&mut command_string).unwrap();
        parse(command_string.trim().to_string()).unwrap_or_else(|()| self.input())
    }
}

struct RobotIO {}

impl Player for RobotIO {
    fn output(&self, message: &Output) {
        // TODO
    }

    fn input(&self) -> Input {
        Input::Next
    }
}

fn parse(string: String) -> Result<Input, ()> {
    match string.to_lowercase().as_str() {
        "next" => Ok(Input::Next),
        "n" => Ok(Input::Next),
        _ => Err(()),
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
    let mut game_state = initial_game_state();
    let players = initialise_players(DEFAULT_OPTIONS);
    while game_state.current_turn < 15 {
        run_turn(&players, &mut game_state);
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
    fn output(&self, message: &Output) {
        self.allies_player.output(message);
        self.empires_player.output(message);
    }

    fn input(&self) -> Input {
        todo!()
    }
}

fn run_turn(players: &Players, game_state: &mut GameState) {
    players.output(&Output::CurrentState(game_state.to_owned()));
    if game_state.current_turn > 1 {
        determine_initiative(players, game_state);
    }
    let inp = players.allies_player.input();
    match inp {
        Input::Next => {
            game_state.current_turn += 1;
        }
        _ => {}
    }
}

/// Decide whose player has the initiative
/// This is only valid when turn > 1 as the empires automatically have the
/// initiative on the first turn
fn determine_initiative(players: &Players, game_state: &mut GameState) {
    players.output(&Output::ChooseInitiative);
    let allies_pr = match players.allies_player.input() {
        Input::Number(pr) => pr,
        _ => 0,
    };
    let empires_pr = match players.empires_player.input() {
        Input::Number(pr) => pr,
        _ => 0,
    };
    let allies_die = 0; //game_state.roll();
    let empires_die = 0; //game_state.roll();

    if allies_die + allies_pr > empires_die + empires_pr {
        game_state.initiative = Side::Empires;
        game_state.reduce_pr(Side::Allies, allies_pr);
        game_state.reduce_pr(Side::Empires, empires_pr);
    }
    players.output(&Output::CurrentState(game_state.to_owned()));
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
enum Nation {
    France,
    Italy,
    Russia,
    Egypt,
    Serbia,
    Romania,
    Greece,
    FrenchAfrica,
    Germany,
    AustriaHungary,
    OttomanEmpire,
    Bulgaria,
    GermanAfrica,
}

impl Display for Nation {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Nation::France => "France",
                Nation::Italy => "Italy",
                Nation::Russia => "Russia",
                Nation::Egypt => "Egypt",
                Nation::Serbia => "Serbia",
                Nation::Romania => "Romania",
                Nation::Greece => "Greece",
                Nation::FrenchAfrica => "French Africa",
                Nation::Germany => "Germany",
                Nation::AustriaHungary => "Austria-Hungary",
                Nation::OttomanEmpire => "Ottoman Empire",
                Nation::Bulgaria => "Bulgaria",
                Nation::GermanAfrica => "German Africa",
            }
        )
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
enum Side {
    Allies,
    Empires,
}

impl Display for Side {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Side::Allies => "Allies",
                Side::Empires => "Empires",
            }
        )
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct Country {
    nation: Nation,
    side: Side,
    max_tech_level: u8,
    resources: u8,
    attack_factor: u8,
}

const COUNTRIES: [Country; 13] = [
    Country {
        nation: Nation::France,
        side: Side::Allies,
        max_tech_level: 7,
        resources: 3,
        attack_factor: 4,
    },
    Country {
        nation: Nation::Italy,
        side: Side::Allies,
        max_tech_level: 3,
        resources: 2,
        attack_factor: 5,
    },
    Country {
        nation: Nation::Russia,
        side: Side::Allies,
        max_tech_level: 3,
        resources: 0,
        attack_factor: 5,
    },
    Country {
        nation: Nation::Egypt,
        side: Side::Allies,
        max_tech_level: 2,
        resources: 2,
        attack_factor: 5,
    },
    Country {
        nation: Nation::Serbia,
        side: Side::Allies,
        max_tech_level: 2,
        resources: 1,
        attack_factor: 5,
    },
    Country {
        nation: Nation::Romania,
        side: Side::Allies,
        max_tech_level: 2,
        resources: 1,
        attack_factor: 5,
    },
    Country {
        nation: Nation::Greece,
        side: Side::Allies,
        max_tech_level: 2,
        resources: 0,
        attack_factor: 5,
    },
    Country {
        nation: Nation::FrenchAfrica,
        side: Side::Allies,
        max_tech_level: 0,
        resources: 2,
        attack_factor: 5,
    },
    Country {
        nation: Nation::Germany,
        side: Side::Empires,
        max_tech_level: 8,
        resources: 4,
        attack_factor: 4,
    },
    Country {
        nation: Nation::AustriaHungary,
        side: Side::Empires,
        max_tech_level: 3,
        resources: 2,
        attack_factor: 5,
    },
    Country {
        nation: Nation::OttomanEmpire,
        side: Side::Empires,
        max_tech_level: 2,
        resources: 2,
        attack_factor: 5,
    },
    Country {
        nation: Nation::Bulgaria,
        side: Side::Empires,
        max_tech_level: 2,
        resources: 1,
        attack_factor: 5,
    },
    Country {
        nation: Nation::GermanAfrica,
        side: Side::Empires,
        max_tech_level: 0,
        resources: 1,
        attack_factor: 5,
    },
];

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
}

impl GameState {
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
}

impl Display for GameState {
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

const INITIAL_BREAKDOWN: [(Nation, u8); 13] = [
    (Nation::France, 7),
    (Nation::Italy, 5),
    (Nation::Russia, 7),
    (Nation::Egypt, 4),
    (Nation::Serbia, 3),
    (Nation::Romania, 3),
    (Nation::Greece, 3),
    (Nation::FrenchAfrica, 4),
    (Nation::Germany, 8),
    (Nation::AustriaHungary, 5),
    (Nation::OttomanEmpire, 5),
    (Nation::Bulgaria, 3),
    (Nation::GermanAfrica, 4),
];

/// Initialises the game state
/// per section 5 of the rulebook
fn initial_game_state() -> GameState {
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
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct Technologies {
    attack: u8,
    defense: u8,
    artillery: u8,
    air: u8,
}

impl Display for Technologies {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "Att {}, Def {}, Art {}, Air {}",
            self.attack, self.defense, self.artillery, self.air
        )
    }
}

fn initial_technologies() -> Technologies {
    Technologies {
        attack: 0,
        defense: 0,
        artillery: 0,
        air: 0,
    }
}

struct Technology {
    name: &'static str,
    date: u16,
    min_dice_unlock: u8,
}

const EMPIRE_TECHNOLOGIES: [[Option<Technology>; 7]; 4] = [
    // Attack
    [
        Some(Technology {
            name: "Combat Gas",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            name: "Firepower",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Stosstruppen",
            date: 1917,
            min_dice_unlock: 6,
        }),
        None,
        None,
        None,
        None,
    ],
    // Defense
    [
        Some(Technology {
            name: "Machine guns",
            date: 1914,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            name: "Trench warfare",
            date: 1915,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Bunkers",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Hindenburg line",
            date: 1917,
            min_dice_unlock: 6,
        }),
        None,
        None,
        None,
    ],
    // Artillery
    [
        Some(Technology {
            name: "Heavy artillery",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            name: "Barrage fire",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Bruchmüller",
            date: 1917,
            min_dice_unlock: 6,
        }),
        None,
        None,
        None,
        None,
    ],
    // Air
    [
        Some(Technology {
            name: "Reco",
            date: 1915,
            min_dice_unlock: 5,
        }),
        None,
        None,
        None,
        Some(Technology {
            name: "Jagdstaffeln",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Fokker D.VII",
            date: 1918,
            min_dice_unlock: 5,
        }),
        None,
    ],
];

const ALLIES_TECHNOLOGIES: [[Option<Technology>; 7]; 4] = [
    // Attack
    [
        Some(Technology {
            name: "Combat Gas",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            name: "New Tactics",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "English Tanks Mark",
            date: 1917,
            min_dice_unlock: 6,
        }),
        Some(Technology {
            name: "French Tanks Renault FT",
            date: 1918,
            min_dice_unlock: 6,
        }),
        None,
        None,
        None,
    ],
    // Defense
    [
        Some(Technology {
            name: "Machine guns",
            date: 1914,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            name: "Trench warfare",
            date: 1915,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Bunkers",
            date: 1916,
            min_dice_unlock: 6,
        }),
        None,
        None,
        None,
        None,
    ],
    // Artillery
    [
        Some(Technology {
            name: "Heavy artillery",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            name: "Barrage fire",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Rolling barrage",
            date: 1916,
            min_dice_unlock: 6,
        }),
        None,
        None,
        None,
        None,
    ],
    // Air
    [
        Some(Technology {
            name: "Reco",
            date: 1915,
            min_dice_unlock: 5,
        }),
        None,
        None,
        Some(Technology {
            name: "Nieuport 11",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            name: "Spad",
            date: 1917,
            min_dice_unlock: 5,
        }),
        None,
        Some(Technology {
            name: "Air division",
            date: 1918,
            min_dice_unlock: 6,
        }),
    ],
];

#[cfg(test)]
mod tests {
    use crate::{initial_game_state, parse, Input::*, Side::*};

    #[test]
    fn parses_next_command() {
        for command in &["next", "n", "N", "Next"] {
            assert_eq!(parse(command.to_string()), Ok(Next));
        }
    }

    #[test]
    fn parses_number() {
        assert_eq!(parse("12".to_string()), Ok(Number(12)));
    }

    #[test]
    fn rejects_invalid_commands() {
        for command in &["ne", "x", ""] {
            assert_eq!(parse(command.to_string()), Err(()));
        }
    }

    #[test]
    fn adjusts_resources_given_a_side_and_some_amount() {
        let mut state = initial_game_state();
        state.increase_pr(Allies, 4);
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
        state.reduce_pr(Allies, 3);
        assert_eq!(1, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn cannot_reduce_resources_below_0() {
        let mut state = initial_game_state();
        state.reduce_pr(Allies, 3);
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }
}
