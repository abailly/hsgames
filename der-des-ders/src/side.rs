use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(Hash, Eq, PartialEq, PartialOrd, Ord, Copy, Clone, Debug)]
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

impl Nation {
    /// Returns whether the nation is adjacent to another nation.
    /// This function only considers _enemies_ as adjacent as its main use is to determine
    /// whether a nation can attack another.
    pub fn adjacent_to(&self, to: &Nation) -> bool {
        match self {
            France => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => true,
                AustriaHungary => false,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => false,
            },
            Italy => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => true,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => false,
            },
            Russia => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => true,
                AustriaHungary => true,
                OttomanEmpire => true,
                Bulgaria => false,
                GermanAfrica => false,
            },
            Egypt => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => true,
                Bulgaria => false,
                GermanAfrica => false,
            },
            Serbia => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => true,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => true,
                OttomanEmpire => false,
                Bulgaria => true,
                GermanAfrica => false,
            },
            Romania => match to {
                France => false,
                Italy => false,
                Russia => true,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => true,
                OttomanEmpire => false,
                Bulgaria => true,
                GermanAfrica => false,
            },
            Greece => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => true,
                Bulgaria => true,
                GermanAfrica => false,
            },
            FrenchAfrica => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => true,
            },
            Germany => match to {
                France => true,
                Italy => false,
                Russia => true,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => false,
            },
            AustriaHungary => match to {
                France => false,
                Italy => true,
                Russia => true,
                Egypt => false,
                Serbia => true,
                Romania => true,
                Greece => false,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => false,
            },
            OttomanEmpire => match to {
                France => false,
                Italy => false,
                Russia => true,
                Egypt => true,
                Serbia => false,
                Romania => false,
                Greece => true,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => false,
            },
            Bulgaria => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => true,
                Romania => true,
                Greece => true,
                FrenchAfrica => false,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => false,
            },
            GermanAfrica => match to {
                France => false,
                Italy => false,
                Russia => false,
                Egypt => false,
                Serbia => false,
                Romania => false,
                Greece => false,
                FrenchAfrica => true,
                Germany => false,
                AustriaHungary => false,
                OttomanEmpire => false,
                Bulgaria => false,
                GermanAfrica => false,
            },
        }
    }

    pub(crate) fn side(&self) -> Side {
        COUNTRIES
            .iter()
            .find(|(nation, _)| nation == self)
            .unwrap()
            .1
            .side
    }

    pub(crate) fn maximum_breakdown(&self) -> u8 {
        COUNTRIES
            .iter()
            .find(|(nation, _)| nation == self)
            .unwrap()
            .1
            .max_breakdown
    }

    pub(crate) fn values() -> impl Iterator<Item = &'static Nation> {
        [
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
        ]
        .iter()
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum NationState {
    AtWar(u8),
    AtPeace,
}

/// Returns the operational level given a breakdown value.
pub fn operational_level(breakdown: u8) -> u8 {
    match breakdown {
        0 => 0,
        1 => 0,
        2 => 1,
        3 => 1,
        4 => 2,
        5 => 2,
        6 => 2,
        7 => 3,
        8 => 3,
        9 => 3,
        _ => panic!("Invalid breakdown value"),
    }
}

impl NationState {
    pub fn operational_level(&self) -> u8 {
        match self {
            AtWar(breakdown) => operational_level(*breakdown),
            AtPeace => 0,
        }
    }

    pub(crate) fn reinforce(&mut self, pr: u8) {
        match self {
            AtWar(breakdown) => *breakdown += pr,
            AtPeace => {}
        }
    }

    pub(crate) fn breakdown(&self) -> u8 {
        match self {
            AtWar(breakdown) => *breakdown,
            AtPeace => 0,
        }
    }

    pub(crate) fn breakdown_level(&self) -> u8 {
        match self {
            AtWar(breakdown) => *breakdown,
            AtPeace => 0,
        }
    }
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
    pub max_breakdown: u8,
    pub vp: u8,
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
            max_breakdown: 7,
            vp: 6,
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
            max_breakdown: 5,
            vp: 2,
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
            max_breakdown: 7,
            vp: 3,
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
            max_breakdown: 4,
            vp: 1,
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
            max_breakdown: 3,
            vp: 1,
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
            max_breakdown: 3,
            vp: 1,
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
            max_breakdown: 3,
            vp: 1,
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
            max_breakdown: 4,
            vp: 1,
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
            max_breakdown: 8,
            vp: 6,
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
            max_breakdown: 5,
            vp: 3,
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
            max_breakdown: 5,
            vp: 2,
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
            max_breakdown: 3,
            vp: 2,
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
            max_breakdown: 4,
            vp: 2,
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

#[cfg(test)]
mod tests {
    use crate::operational_level;

    #[test]
    fn operational_level_depends_on_breakdown_value() {
        assert_eq!(0, operational_level(0));
        assert_eq!(1, operational_level(3));
        assert_eq!(3, operational_level(7));
    }
}
