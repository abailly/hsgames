use core::fmt;
use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

trait Output {
    fn output(&self);
}

impl Output for String {
    fn output(&self) {
        println!("{}", self);
    }
}

fn input(prompt: &str) -> Command {
    prompt.to_string().output();
    let mut command_string: String = String::new();
    std::io::stdin().read_line(&mut command_string).unwrap();
    parse(command_string.trim().to_string()).unwrap_or_else(|()| {
        "Invalid command".to_string().output();
        input(prompt)
    })
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Command {
    Next,
}

fn parse(string: String) -> Result<Command, ()> {
    match string.to_lowercase().as_str() {
        "next" => Ok(Command::Next),
        "n" => Ok(Command::Next),
        _ => Err(()),
    }
}

fn main() {
    let mut game_state = initial_game_state();
    while game_state.current_turn < 15 {
        game_state.output();
        let command = input("Next turn");
        match command {
            Command::Next => {
                game_state.current_turn += 1;
            }
        }
    }
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
    russian_revolution: u8,
    breakdown: Box<HashMap<Nation, u8>>,
    state_of_war: Box<HashMap<Side, WarState>>,
}

impl Output for GameState {
    fn output(&self) {
        println!("Turn: {}", self.current_turn);
        println!("Russian Revolution: {}", self.russian_revolution);
        println!("Breakdown:");
        for (nation, resources) in self.breakdown.iter() {
            println!("{}: {}", nation, resources);
        }
        println!("State of War:");
        for (side, war_state) in self.state_of_war.iter() {
            println!("{}:", side);
            println!("Resources: {}", war_state.resources);
            println!("VP: {}", war_state.vp);
            println!("Technologies:");
            println!("Attack: {}", war_state.technologies.attack);
            println!("Defense: {}", war_state.technologies.defense);
            println!("Artillery: {}", war_state.technologies.artillery);
            println!("Air: {}", war_state.technologies.air);
        }
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
    use crate::{parse, Command::*};

    #[test]
    fn parses_valid_commands() {
        for command in &["next", "n", "N", "Next"] {
            assert_eq!(parse(command.to_string()), Ok(Next));
        }
    }

    #[test]
    fn rejects_invalid_commands() {
        for command in &["ne", "x", ""] {
            assert_eq!(parse(command.to_string()), Err(()));
        }
    }
}
