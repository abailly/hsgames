use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Nation {
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
pub enum Side {
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
pub struct Country {
    nation: Nation,
    side: Side,
    max_tech_level: u8,
    resources: u8,
    attack_factor: u8,
}

pub const COUNTRIES: [Country; 13] = [
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

pub const INITIAL_BREAKDOWN: [(Nation, u8); 13] = [
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
