use core::fmt;
use std::fmt::{Display, Formatter};

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum TechnologyType {
    Attack,
    Defense,
    Artillery,
    Air,
}

/// Current technologies of a side
/// Note these numbers are not the actual technology levels but 1-based indices into the
/// side's technology list (see EMPIRE_TECHNOLOGIES and ALLIES_TECHNOLOGIES).
#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Technologies {
    pub attack: u8,
    pub defense: u8,
    pub artillery: u8,
    pub air: u8,
}

pub type TechEffects = (u8, i8, i8, u8);

#[allow(unused_must_use)]
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

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct Technology {
    pub category: TechnologyType,
    #[allow(dead_code)]
    pub level: u8,
    #[allow(dead_code)]
    pub name: &'static str,
    pub date: u16,
    pub min_dice_unlock: u8,
}

pub const EMPIRE_TECHNOLOGIES: [[Option<Technology>; 4]; 4] = [
    // Attack
    [
        Some(Technology {
            category: TechnologyType::Attack,
            level: 1,
            name: "Combat Gas",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            category: TechnologyType::Attack,
            level: 2,
            name: "Firepower",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Attack,
            level: 3,
            name: "Stosstruppen",
            date: 1917,
            min_dice_unlock: 6,
        }),
        None,
    ],
    // Defense
    [
        Some(Technology {
            category: TechnologyType::Defense,
            level: 1,
            name: "Machine guns",
            date: 1914,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            category: TechnologyType::Defense,
            level: 2,
            name: "Trench warfare",
            date: 1915,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Defense,
            level: 3,
            name: "Bunkers",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Defense,
            level: 4,
            name: "Hindenburg line",
            date: 1917,
            min_dice_unlock: 6,
        }),
    ],
    // Artillery
    [
        Some(Technology {
            category: TechnologyType::Artillery,
            level: 1,
            name: "Heavy artillery",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            category: TechnologyType::Artillery,
            level: 2,
            name: "Barrage fire",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Artillery,
            level: 3,
            name: "Bruchmüller",
            date: 1917,
            min_dice_unlock: 6,
        }),
        None,
    ],
    // Air
    [
        Some(Technology {
            category: TechnologyType::Air,
            level: 1,
            name: "Reco",
            date: 1915,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Air,
            level: 5,
            name: "Jagdstaffeln",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Air,
            level: 6,
            name: "Fokker D.VII",
            date: 1918,
            min_dice_unlock: 5,
        }),
        None,
    ],
];

pub const ALLIES_TECHNOLOGIES: [[Option<Technology>; 4]; 4] = [
    // Attack
    [
        Some(Technology {
            category: TechnologyType::Attack,
            level: 1,
            name: "Combat Gas",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            category: TechnologyType::Attack,
            level: 2,
            name: "New Tactics",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Attack,
            level: 3,
            name: "English Tanks Mark",
            date: 1917,
            min_dice_unlock: 6,
        }),
        Some(Technology {
            category: TechnologyType::Attack,
            level: 4,
            name: "French Tanks Renault FT",
            date: 1918,
            min_dice_unlock: 6,
        }),
    ],
    // Defense
    [
        Some(Technology {
            category: TechnologyType::Defense,
            level: 1,
            name: "Machine guns",
            date: 1914,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            category: TechnologyType::Defense,
            level: 2,
            name: "Trench warfare",
            date: 1915,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Defense,
            level: 3,
            name: "Bunkers",
            date: 1916,
            min_dice_unlock: 6,
        }),
        None,
    ],
    // Artillery
    [
        Some(Technology {
            category: TechnologyType::Artillery,
            level: 1,
            name: "Heavy artillery",
            date: 1915,
            min_dice_unlock: 4,
        }),
        Some(Technology {
            category: TechnologyType::Artillery,
            level: 2,
            name: "Barrage fire",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Artillery,
            level: 3,
            name: "Rolling barrage",
            date: 1916,
            min_dice_unlock: 6,
        }),
        None,
    ],
    // Air
    [
        Some(Technology {
            category: TechnologyType::Air,
            level: 1,
            name: "Reco",
            date: 1915,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Air,
            level: 4,
            name: "Nieuport 11",
            date: 1916,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Air,
            level: 5,
            name: "Spad",
            date: 1917,
            min_dice_unlock: 5,
        }),
        Some(Technology {
            category: TechnologyType::Air,
            level: 7,
            name: "Air division",
            date: 1918,
            min_dice_unlock: 6,
        }),
    ],
];
