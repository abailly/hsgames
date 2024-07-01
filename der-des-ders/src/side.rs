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

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum NationState {
    AtWar(u8),
    AtPeace,
}

impl Display for NationState {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            AtWar(breakdown) => write!(f, "At war: {}", breakdown),
            AtPeace => write!(f, "At peace"),
        }
    }
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

impl Side {
    pub(crate) fn other(&self) -> Side {
        match self {
            Side::Allies => Side::Empires,
            Side::Empires => Side::Allies,
        }
    }
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
    pub nation: Nation,
    pub side: Side,
    pub max_tech_level: u8,
    pub resources: u8,
    pub attack_factor: u8,
}

use Nation::*;

pub const COUNTRIES: [(Nation, Country); 13] = [
    (
        France,
        Country {
            nation: France,
            side: Side::Allies,
            max_tech_level: 7,
            resources: 3,
            attack_factor: 4,
        },
    ),
    (
        Italy,
        Country {
            nation: Italy,
            side: Side::Allies,
            max_tech_level: 3,
            resources: 2,
            attack_factor: 5,
        },
    ),
    (
        Russia,
        Country {
            nation: Russia,
            side: Side::Allies,
            max_tech_level: 3,
            resources: 0,
            attack_factor: 5,
        },
    ),
    (
        Egypt,
        Country {
            nation: Egypt,
            side: Side::Allies,
            max_tech_level: 2,
            resources: 2,
            attack_factor: 5,
        },
    ),
    (
        Serbia,
        Country {
            nation: Serbia,
            side: Side::Allies,
            max_tech_level: 2,
            resources: 1,
            attack_factor: 5,
        },
    ),
    (
        Romania,
        Country {
            nation: Romania,
            side: Side::Allies,
            max_tech_level: 2,
            resources: 1,
            attack_factor: 5,
        },
    ),
    (
        Greece,
        Country {
            nation: Greece,
            side: Side::Allies,
            max_tech_level: 2,
            resources: 0,
            attack_factor: 5,
        },
    ),
    (
        FrenchAfrica,
        Country {
            nation: FrenchAfrica,
            side: Side::Allies,
            max_tech_level: 0,
            resources: 2,
            attack_factor: 5,
        },
    ),
    (
        Germany,
        Country {
            nation: Germany,
            side: Side::Empires,
            max_tech_level: 8,
            resources: 4,
            attack_factor: 4,
        },
    ),
    (
        AustriaHungary,
        Country {
            nation: AustriaHungary,
            side: Side::Empires,
            max_tech_level: 3,
            resources: 2,
            attack_factor: 5,
        },
    ),
    (
        OttomanEmpire,
        Country {
            nation: OttomanEmpire,
            side: Side::Empires,
            max_tech_level: 2,
            resources: 2,
            attack_factor: 5,
        },
    ),
    (
        Bulgaria,
        Country {
            nation: Bulgaria,
            side: Side::Empires,
            max_tech_level: 2,
            resources: 1,
            attack_factor: 5,
        },
    ),
    (
        GermanAfrica,
        Country {
            nation: GermanAfrica,
            side: Side::Empires,
            max_tech_level: 0,
            resources: 1,
            attack_factor: 5,
        },
    ),
];

use NationState::*;

pub const INITIAL_NATION_STATE: [(Nation, NationState); 13] = [
    (France, AtWar(7)),
    (Italy, AtPeace),
    (Russia, AtWar(7)),
    (Egypt, AtWar(4)),
    (Serbia, AtWar(3)),
    (Romania, AtPeace),
    (Greece, AtPeace),
    (FrenchAfrica, AtWar(4)),
    (Germany, AtWar(8)),
    (AustriaHungary, AtWar(5)),
    (OttomanEmpire, AtWar(5)),
    (Bulgaria, AtPeace),
    (GermanAfrica, AtWar(4)),
];
