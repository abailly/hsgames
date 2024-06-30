use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum TechnologyType {
    Attack,
    Defense,
    Artillery,
    Air,
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Technologies {
    pub attack: u8,
    pub defense: u8,
    pub artillery: u8,
    pub air: u8,
}

pub const ZERO_TECHNOLOGIES: Technologies = Technologies {
    attack: 0,
    defense: 0,
    artillery: 0,
    air: 0,
};

impl Display for Technologies {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "Att {}, Def {}, Art {}, Air {}",
            self.attack, self.defense, self.artillery, self.air
        )
    }
}

pub fn initial_technologies() -> Technologies {
    Technologies {
        attack: 0,
        defense: 0,
        artillery: 0,
        air: 0,
    }
}

pub struct Technology {
    pub name: &'static str,
    pub date: u16,
    pub min_dice_unlock: u8,
}

pub const EMPIRE_TECHNOLOGIES: [[Option<Technology>; 7]; 4] = [
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

pub const ALLIES_TECHNOLOGIES: [[Option<Technology>; 7]; 4] = [
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
