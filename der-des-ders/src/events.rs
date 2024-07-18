use crate::logic::*;
use crate::side::*;
use crate::state::*;

impl GameLogic for RaceToTheSea {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        if self.active
            && (offensive.from == Nation::France && offensive.to == Nation::Germany
                || (offensive.from == Nation::Germany && offensive.to == Nation::France))
        {
            let (artillery_bonus, attack_bonus, defense_malus) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus + 1, defense_malus)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct RaceToTheSea {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl RaceToTheSea {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        RaceToTheSea {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl<T: GameLogic> GameLogic for ShellCrisis<T> {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        if self.active && offensive.from == Nation::France {
            let (artillery_bonus, attack_bonus, defense_malus) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus - 1, defense_malus)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct ShellCrisis<T: GameLogic> {
    pub active: bool,
    pub previous: Box<T>,
}

impl<T: GameLogic> ShellCrisis<T> {
    pub fn new(previous: T) -> Self {
        ShellCrisis {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for Gas {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }

    fn roll_artillery_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        let roll_artillery_dice = self.previous.roll_artillery_dice(state, num);
        if let Some(offensive) = &mut self.offensive {
            if offensive.from == Nation::Germany {
                roll_artillery_dice.into_iter().map(|die| die + 1).collect()
            } else {
                roll_artillery_dice
            }
        } else {
            roll_artillery_dice
        }
    }
}

pub struct Gas {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
    pub offensive: Option<Offensive>,
}

impl Gas {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        Gas {
            active: true,
            offensive: None,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for VonLettowInAfrica {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        if state.operational_level(&Nation::GermanAfrica) == 0 {
            self.active = false;
        }
        if self.active {
            self.offensive = Some(offensive.clone());
        } else {
            self.offensive = None;
        }
        self.previous.compute_bonus(state, offensive)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        let added_die = if let Some(offensive) = &self.offensive {
            if offensive.from == Nation::GermanAfrica {
                1
            } else {
                0
            }
        } else {
            0
        };
        self.previous.roll_offensive_dice(state, num + added_die)
    }
}

pub struct VonLettowInAfrica {
    pub offensive: Option<Offensive>,
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl VonLettowInAfrica {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        VonLettowInAfrica {
            offensive: None,
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for Gallipoli {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn collect_resources(&mut self, state: &mut GameState) {
        self.previous.collect_resources(state);
        if self.active {
            let die = state.roll();
            self.reduce_pr(state, &Side::Allies, die);
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.previous.new_turn(state);
        self.active = false;
    }
}

pub struct Gallipoli {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl Gallipoli {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        Gallipoli {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for GermanFleetDefeated {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> u8 {
        let die = (state.roll() - 1 + bonus).max(1);
        println!("Uboot losses: {}", die);
        match die {
            1..=4 => 0,
            5 => 2,
            _ => 4,
        }
    }
}

pub struct GermanFleetDefeated {
    pub previous: Box<dyn GameLogic>,
}

impl GermanFleetDefeated {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        GermanFleetDefeated {
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for GermanFleetDestroyed {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn blockade_effect(&mut self, _state: &mut GameState, _bonus: u8) -> u8 {
        0
    }
}

pub struct GermanFleetDestroyed {
    pub previous: Box<dyn GameLogic>,
}

impl GermanFleetDestroyed {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        GermanFleetDestroyed {
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for SeparatePeace {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        self.offensive = Some(offensive.clone());
        self.previous.compute_bonus(state, offensive)
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.previous.new_turn(state);
        self.active = false;
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        if self.active {
            let added_die = if let Some(offensive) = &self.offensive {
                if (offensive.from == Nation::Germany || offensive.from == Nation::AustriaHungary)
                    && offensive.to == Nation::Russia
                {
                    1
                } else {
                    0
                }
            } else {
                0
            };
            self.previous.roll_offensive_dice(state, num + added_die)
        } else {
            self.previous.roll_offensive_dice(state, num)
        }
    }
}

pub struct SeparatePeace {
    pub offensive: Option<Offensive>,
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl SeparatePeace {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        SeparatePeace {
            offensive: None,
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for TrentinOffensive {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        if self.active && offensive.from == Nation::AustriaHungary && offensive.to == Nation::Italy
        {
            let (artillery_bonus, attack_bonus, defense_malus) =
                self.previous.compute_bonus(state, offensive);
            (artillery_bonus, attack_bonus + 1, defense_malus)
        } else {
            self.previous.compute_bonus(state, offensive)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        self.active = false;
        self.previous.new_turn(state);
    }
}

pub struct TrentinOffensive {
    pub active: bool,
    pub previous: Box<dyn GameLogic>,
}

impl TrentinOffensive {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        TrentinOffensive {
            active: true,
            previous: Box::new(previous),
        }
    }
}

impl GameLogic for WoodrowWilson {
    fn previous(&mut self) -> Option<&mut dyn GameLogic> {
        Some(&mut *self.previous)
    }

    fn uboot_losses(&mut self, _state: &mut GameState, _bonus: u8) -> u8 {
        0
    }
}

pub struct WoodrowWilson {
    pub previous: Box<dyn GameLogic>,
}

impl WoodrowWilson {
    pub fn new(previous: Box<dyn GameLogic>) -> Self {
        WoodrowWilson {
            previous: Box::new(previous),
        }
    }
}

#[cfg(test)]
mod game_events_tests {

    use crate::{
        event::ALL_EVENTS, fixtures::EngineBuilder, Nation::*, NationState, Offensive, Side::*,
    };

    #[test]
    fn shell_crisis_event_inflicts_minus_1_to_france_attacks() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Shells crisis"
        engine.play_events(&ALL_EVENTS[4]);
        let (_, att, _) = engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: France,
            to: Germany,
            pr: 2,
        });

        assert_eq!(-1, att);
    }

    #[test]
    fn gas_event_adds_1_to_germany_artillery_rolls() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Gas!"
        engine.play_events(&ALL_EVENTS[5]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: Germany,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_artillery_dice(2);

        assert_eq!(vec![3, 3], dice);
    }

    #[test]
    fn gas_event_does_not_add_to_austria_artillery_rolls() {
        let mut engine = EngineBuilder::new(11).build();

        engine.play_events(&ALL_EVENTS[5]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: AustriaHungary,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_artillery_dice(2);

        assert_eq!(vec![2, 2], dice);
    }

    #[test]
    fn von_lettow_adds_one_dice_for_offensive_in_africa() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Von Lettow in Africa"
        engine.play_events(&ALL_EVENTS[6]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: GermanAfrica,
            to: FrenchAfrica,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![2, 2, 4], dice);
    }

    #[test]
    fn von_lettow_does_node_add_die_once_operational_level_falls_to_0() {
        let mut engine = EngineBuilder::new(11)
            .with_nation(GermanAfrica, NationState::AtWar(1))
            .build();

        // activate "Von Lettow in Africa"
        engine.play_events(&ALL_EVENTS[6]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: GermanAfrica,
            to: FrenchAfrica,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![2, 2], dice);
    }

    #[test]
    fn von_lettow_does_not_add_to_offensive_outside_africa() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Von Lettow in Africa"
        engine.play_events(&ALL_EVENTS[6]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: AustriaHungary,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![2, 2], dice);
    }

    #[test]
    fn gallipoli_reduces_pr_by_1_die_for_allies() {
        let mut engine = EngineBuilder::new(11).build();

        engine.play_events(&ALL_EVENTS[7]);
        engine.collect_resources();

        assert_eq!(12, engine.state.resources_for(&Allies));
    }

    #[test]
    fn separate_peace_adds_1_die_to_offensive_from_germany_to_russia() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[8]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: Germany,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![3, 3, 5], dice);
    }

    #[test]
    fn separate_peace_adds_1_die_to_offensive_from_austria_to_russia() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[8]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: AustriaHungary,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![3, 3, 5], dice);
    }

    #[test]
    fn separate_peace_doesn_not_add_die_to_offensive_from_turkey_to_russia() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[8]);
        engine.compute_bonus(&Offensive {
            initiative: Empires,
            from: OttomanEmpire,
            to: Russia,
            pr: 2,
        });
        let dice = engine.roll_offensive_dice(2);

        assert_eq!(vec![3, 3], dice);
    }

    #[test]
    fn italy_can_enter_war_with_event() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[9]);

        assert_eq!(
            NationState::AtWar(5),
            *engine.state.nations.get(&Italy).unwrap()
        );
    }

    #[test]
    fn bulgaria_can_enter_war_with_event() {
        let mut engine = EngineBuilder::new(12).build();

        engine.play_events(&ALL_EVENTS[10]);

        assert_eq!(
            NationState::AtWar(3),
            *engine.state.nations.get(&Bulgaria).unwrap()
        );
    }

    #[test]
    fn battle_of_jutland_adds_3_pr_to_empires_on_roll_of_1() {
        let mut engine = EngineBuilder::new(6).build(); // die roll = 1

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(3, engine.state.resources_for(&Empires),);
    }

    #[test]
    fn battle_of_jutland_on_roll_of_5_inflicts_penalty_of_1_to_uboot_rolls() {
        let mut engine = EngineBuilder::new(116).with_resources(Allies, 6).build(); // die roll = 5 , 6

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(2, engine.uboot_losses(0));
    }

    #[test]
    fn battle_of_jutland_on_roll_of_3_has_no_effect() {
        let mut engine = EngineBuilder::new(30).with_resources(Allies, 6).build(); // die roll = 3 , 6

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(4, engine.uboot_losses(0));
    }

    #[test]
    fn battle_of_jutland_on_roll_of_6_cancels_blockade_roll() {
        let mut engine = EngineBuilder::new(22).with_resources(Allies, 6).build(); // die roll = 6, 1

        engine.play_events(&ALL_EVENTS[13]);

        assert_eq!(0, engine.blockade_effect(1));
        assert_eq!(6, engine.state.resources_for(&Allies));
    }

    #[test]
    fn trentin_offensive_adds_1_to_austria_attack_on_italy() {
        let mut engine = EngineBuilder::new(11).build();

        engine.play_events(&ALL_EVENTS[14]);
        let (_, att, _) = engine.compute_bonus(&Offensive {
            initiative: Allies,
            from: AustriaHungary,
            to: Italy,
            pr: 2,
        });

        assert_eq!(1, att);
    }

    #[test]
    fn woodrow_wilson_cancels_uboot_roll() {
        let mut engine = EngineBuilder::new(22).with_resources(Empires, 2).build(); // die roll = 6

        engine.play_events(&ALL_EVENTS[15]);

        assert_eq!(0, engine.uboot_losses(1));
        assert_eq!(2, engine.state.resources_for(&Empires));
    }
}

// 0 : 5 4 5
// 1 : 5 2 3
// 2 : 1 6 1
// 3 : 1 3 2
// 4 : 5 4 1
// 5 : 2 3 2
// 6 : 1 3 5
// 7 : 3 1 1
// 8 : 3 6 5
// 9 : 4 1 5
// 10 : 3 3 2
// 11 : 2 2 4
// 12 : 3 5 4
// 13 : 4 4 4
// 14 : 6 4 6
// 15 : 4 6 2
// 16 : 6 6 3
// 17 : 4 2 2
// 18 : 1 3 2
// 19 : 5 1 3
// 20 : 3 1 5
// 21 : 6 1 3
// 22 : 6 1 1
// 23 : 2 3 6
// 24 : 3 2 4
// 25 : 4 3 2
// 26 : 2 2 4
// 27 : 6 2 2
// 28 : 3 3 1
// 29 : 2 5 3
// 30 : 3 6 3
// 31 : 2 2 4
// 32 : 3 2 6
// 33 : 5 2 4
// 34 : 5 3 3
// 35 : 4 5 2
// 36 : 3 4 6
// 37 : 1 4 4
// 38 : 3 2 3
// 39 : 4 6 6
// 40 : 1 5 1
// 41 : 2 6 2
// 42 : 4 2 4
// 43 : 6 5 3
// 44 : 3 5 3
// 45 : 5 3 1
// 46 : 3 2 1
// 47 : 6 3 2
// 48 : 3 1 3
// 49 : 1 6 6
// 50 : 4 5 6
// 51 : 2 3 2
// 52 : 2 2 6
// 53 : 5 3 4
// 54 : 3 1 5
// 55 : 2 1 3
// 56 : 6 2 2
// 57 : 5 2 6
// 58 : 4 2 3
// 59 : 2 2 6
// 60 : 6 3 3
// 61 : 3 3 1
// 62 : 6 4 6
// 63 : 4 6 1
// 64 : 5 4 2
// 65 : 6 1 1
// 66 : 5 4 3
// 67 : 5 3 1
// 68 : 3 5 6
// 69 : 2 1 5
// 70 : 1 4 2
// 71 : 3 2 1
// 72 : 4 2 6
// 73 : 5 5 4
// 74 : 4 4 5
// 75 : 3 6 3
// 76 : 2 6 3
// 77 : 3 4 4
// 78 : 1 2 4
// 79 : 6 5 6
// 80 : 3 4 5
// 81 : 6 1 6
// 82 : 1 1 5
// 83 : 2 5 2
// 84 : 4 1 2
// 85 : 3 1 2
// 86 : 1 5 3
// 87 : 6 4 5
// 88 : 4 6 6
// 89 : 1 4 5
// 90 : 5 3 5
// 91 : 2 1 3
// 92 : 1 4 4
// 93 : 4 1 4
// 94 : 2 4 2
// 95 : 5 5 4
// 96 : 6 4 5
// 97 : 3 4 5
// 98 : 4 4 1
// 99 : 5 5 3
// 100 : 5 4 6
// 101 : 5 2 1
// 102 : 2 5 1
// 103 : 2 6 5
// 104 : 1 5 6
// 105 : 5 2 1
// 106 : 6 6 4
// 107 : 2 3 1
// 108 : 1 4 2
// 109 : 3 3 3
// 110 : 2 4 1
// 111 : 3 2 1
// 112 : 6 1 2
// 113 : 1 6 4
// 114 : 2 3 4
// 115 : 3 4 4
// 116 : 5 6 2
// 117 : 5 4 3
// 118 : 3 6 2
// 119 : 3 5 3
// 120 : 1 4 2
// 121 : 1 1 2
// 122 : 6 6 4
// 123 : 2 5 6
// 124 : 4 4 3
// 125 : 4 6 5
// 126 : 4 5 4
// 127 : 2 2 5
// 128 : 6 2 4
// 129 : 3 6 3
// 130 : 4 4 5
// 131 : 6 1 6
// 132 : 4 4 6
// 133 : 4 2 4
// 134 : 3 5 4
// 135 : 3 5 3
// 136 : 2 6 3
// 137 : 3 2 2
// 138 : 4 6 3
// 139 : 5 4 6
// 140 : 5 4 2
// 141 : 2 2 6
// 142 : 1 5 6
// 143 : 4 6 2
// 144 : 6 6 4
// 145 : 4 6 6
// 146 : 5 3 6
// 147 : 1 3 6
// 148 : 1 1 2
// 149 : 4 3 1
// 150 : 6 5 1
// 151 : 5 6 5
// 152 : 2 3 4
// 153 : 3 2 4
// 154 : 5 2 1
// 155 : 2 6 2
// 156 : 6 2 6
// 157 : 4 6 6
// 158 : 5 4 2
// 159 : 4 5 4
// 160 : 4 1 6
// 161 : 4 2 1
// 162 : 3 2 2
// 163 : 4 1 1
// 164 : 2 2 5
// 165 : 6 3 3
// 166 : 3 1 1
// 167 : 3 1 1
// 168 : 6 6 3
// 169 : 5 3 1
// 170 : 2 3 3
// 171 : 6 4 1
// 172 : 3 3 2
// 173 : 3 1 6
// 174 : 6 6 4
// 175 : 4 3 3
// 176 : 2 5 2
// 177 : 6 3 3
// 178 : 4 3 2
// 179 : 4 3 4
// 180 : 3 4 5
// 181 : 3 4 2
// 182 : 6 1 5
// 183 : 4 1 6
// 184 : 5 5 4
// 185 : 3 6 1
// 186 : 1 2 2
// 187 : 6 6 5
// 188 : 3 4 5
// 189 : 3 3 3
// 190 : 3 1 4
// 191 : 5 6 1
// 192 : 5 4 2
// 193 : 5 1 3
// 194 : 5 5 1
// 195 : 2 5 3
// 196 : 4 3 6
// 197 : 5 5 3
// 198 : 1 5 3
// 199 : 5 4 4
