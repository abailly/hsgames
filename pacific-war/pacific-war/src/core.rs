use chrono::NaiveDate;
use fastrand::Rng;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Display;
use uuid::Uuid;

use crate::util::*;

use CombatDistance::*;
use Lighting::*;
use MovementType::*;
use Side::*;

#[derive(Debug, PartialEq, Serialize, Deserialize, FromFormField, Clone, Copy)]
pub enum Side {
    Japan,
    Allies,
}

impl Side {
    fn opposite(&self) -> Side {
        match self {
            Japan => Allies,
            Allies => Japan,
        }
    }
}

impl Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Japan => write!(f, "japan"),
            Allies => write!(f, "allies"),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, FromFormField, Clone)]
pub enum Intelligence {
    Surprise,
    Intercept,
    Ambush,
    AmbushCV,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, FromForm, Clone)]
pub struct NewBattle {
    pub battle_name: String,
    pub start_date: NaiveDateForm,
    pub duration: u8,
    pub operation_player: Side,
    pub intelligence_condition: Intelligence,
}

impl NewBattle {
    pub fn to_form(&self) -> String {
        format!(
            "battle_name={}&start_date={}&duration={}&operation_player={}&intelligence_condition={:?}",
            self.battle_name, self.start_date.date, self.duration, self.operation_player, self.intelligence_condition
        )
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct Battle {
    pub id: Uuid,
    pub battle_data: NewBattle,
    pub current_date: NaiveDate,
    pub phase: Phase,
}

impl Battle {
    pub fn new(id: Uuid, new_battle: &NewBattle) -> Self {
        Battle {
            id,
            battle_data: new_battle.clone(),
            current_date: new_battle.start_date.date.clone(),
            phase: Phase::OperationContactPhase(ContactPhase::new()),
        }
    }

    pub fn contact_movement(&mut self, movement: &MovementType) {
        match &mut self.phase {
            Phase::OperationContactPhase(phase) => {
                let add_day = update_contact_phase(phase, movement);
                if add_day {
                    self.current_date = self.current_date.succ();
                }
            }
            Phase::ReactionContactPhase(phase) => {
                update_contact_phase(phase, movement);
            }
            _ => {}
        }
    }

    pub fn next(&mut self) {
        match &mut self.phase {
            Phase::OperationContactPhase(phase) => match self.battle_data.intelligence_condition {
                Intelligence::Ambush | Intelligence::AmbushCV => {
                    self.phase = Phase::ReactionContactPhase(ContactPhase {
                        max_naval_movement_count: phase.naval_movement_count * 2,
                        ..ContactPhase::new()
                    });
                }
                _ => {
                    self.phase = Phase::ReactionContactPhase(ContactPhase {
                        max_naval_movement_count: phase.naval_movement_count,
                        ..ContactPhase::new()
                    });
                }
            },
            Phase::ReactionContactPhase(_) => {
                self.phase = Phase::BattleCyclePhase(BattleCycle::new(
                    self.battle_data.intelligence_condition.clone(),
                    self.id.as_u128(),
                ));
            }
            Phase::BattleCyclePhase(battle_cycle) => {
                let count = battle_cycle.count;
                battle_cycle.next();
                if battle_cycle.count != count {
                    self.current_date = self.current_date.succ().succ();
                }
            }
        }
    }

    pub fn determine_advantage(&mut self) {
        match &mut self.phase {
            Phase::BattleCyclePhase(battle_cycle) => {
                battle_cycle.determine_advantage(self.battle_data.operation_player);
            }
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Phase {
    OperationContactPhase(ContactPhase),
    ReactionContactPhase(ContactPhase),
    BattleCyclePhase(BattleCycle),
}

impl Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Phase::OperationContactPhase(_) => write!(f, "Operation Contact Phase"),
            Phase::ReactionContactPhase(_) => write!(f, "Reaction Contact Phase"),
            Phase::BattleCyclePhase(_) => write!(f, "Battle Cycle"),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct ContactPhase {
    pub remaining: Vec<MovementType>,
    pub current: Option<MovementType>,
    /// Maximum number of naval movements allowed.
    ///
    /// Defaults to u8::MAX for Operation player, and then is set to
    /// whatever number of hexes operation player moved for reaction
    /// player, or twice in case of ambush intelligence condition
    pub max_naval_movement_count: u8,
    pub naval_movement_count: u8,
}

impl ContactPhase {
    pub fn new() -> Self {
        ContactPhase {
            remaining: vec![GroundMovement, AirMovement, NavalMovement],
            max_naval_movement_count: u8::MAX,
            current: None,
            naval_movement_count: 0,
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum MovementType {
    GroundMovement,
    AirMovement,
    NavalMovement,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct BattleCycle {
    pub lighting_condition: Option<Lighting>,
    pub lighting_chosen: bool,
    pub intelligence_condition: Intelligence,
    pub advantage_player: Option<Side>,
    pub count: u8,
    pub phase: BattleCycleSegment,
    seed: u64,
}

impl BattleCycle {
    pub fn new(intelligence_condition: Intelligence, seed: u128) -> Self {
        BattleCycle {
            lighting_condition: None,
            lighting_chosen: false,
            advantage_player: None,
            intelligence_condition,
            count: 1,
            phase: BattleCycleSegment::SetLighting,
            seed: seed as u64,
        }
    }

    pub fn intercept(seed: u128) -> Self {
        BattleCycle::new(Intelligence::Intercept, seed)
    }

    fn next(&mut self) -> &mut Self {
        match self.phase {
            BattleCycleSegment::SetLighting => {
                self.phase = BattleCycleSegment::AdvantageDetermination;
            }
            BattleCycleSegment::AdvantageDetermination => {
                self.phase = BattleCycleSegment::AdvantageMovement(BattleMovementPhase::new());
            }
            BattleCycleSegment::AdvantageMovement(_) => {
                self.phase = BattleCycleSegment::AdvantageAirMission;
            }
            BattleCycleSegment::AdvantageAirMission => {
                self.phase = BattleCycleSegment::NavalCombats(NavalCombat::new(1));
            }
            BattleCycleSegment::NavalCombats(_) => {
                self.phase = BattleCycleSegment::Bombardment;
            }
            BattleCycleSegment::Bombardment => {
                self.phase = BattleCycleSegment::Demolition;
            }
            BattleCycleSegment::Demolition => {
                self.phase = BattleCycleSegment::GroundCombat;
            }
            BattleCycleSegment::GroundCombat => {
                self.phase = BattleCycleSegment::AirBaseRepair;
            }
            BattleCycleSegment::AirBaseRepair => {
                self.phase = BattleCycleSegment::Rally;
            }
            BattleCycleSegment::Rally => {
                self.phase = BattleCycleSegment::DisadvantageMovement(BattleMovementPhase::new());
            }
            BattleCycleSegment::DisadvantageMovement(_) => {
                self.phase = BattleCycleSegment::DisadvantageAirMission;
            }
            BattleCycleSegment::DisadvantageAirMission => {
                self.phase = BattleCycleSegment::ActivationDeactivation;
            }
            BattleCycleSegment::ActivationDeactivation => {
                self.phase = BattleCycleSegment::DetectionRemoval;
            }
            BattleCycleSegment::DetectionRemoval => {
                self.phase = BattleCycleSegment::DayAdjustment;
            }
            BattleCycleSegment::DayAdjustment => {
                self.phase = BattleCycleSegment::SetLighting;
                self.count += 1;
            }
        }
        self
    }

    pub fn next_round(&mut self) -> &mut Self {
        if let BattleCycleSegment::NavalCombats(combat) = &mut self.phase {
            combat.next_round();
        }
        self
    }

    pub fn choose_lighting(&mut self, lighting: Lighting) {
        self.lighting_condition = Some(lighting);
        self.lighting_chosen = true;
    }

    pub fn random_lighting(&mut self) {
        let mut rng = Rng::with_seed(self.seed);
        let lighting = match rng.u8(0..10) {
            0..=1 => Night,
            2 => Dusk,
            _ => DayPM,
        };
        self.lighting_condition = Some(lighting);
        self.seed = rng.get_seed();
    }

    pub fn next_lighting(&mut self) -> &mut Self {
        match self.lighting_condition {
            Some(DayAM) => self.random_lighting(),
            Some(DayPM) => self.lighting_condition = Some(Dusk),
            Some(Dusk) => self.lighting_condition = Some(Night),
            Some(Night) => self.lighting_condition = Some(DayAM),
            None => self.random_lighting(),
        }
        self
    }

    pub fn operation_advance_lighting(&mut self) -> &mut Self {
        self.lighting_chosen = true;
        self.next_lighting().next_lighting()
    }

    pub fn can_reaction_player_choose_lighting(&self) -> bool {
        self.count == 1
            && (self.intelligence_condition == Intelligence::Ambush
                || self.intelligence_condition == Intelligence::AmbushCV)
    }

    pub fn can_operation_player_advance_lighting(&self) -> bool {
        self.count > 1
            && (self.intelligence_condition == Intelligence::Surprise
                || self.intelligence_condition == Intelligence::Intercept)
            && !self.lighting_chosen
    }

    fn determine_advantage(&mut self, operation_player: Side) {
        if self.count == 1 {
            self.determine_advantage_first_cycle(operation_player);
        } else {
            self.determine_advantage_other_cycle(operation_player);
        }
    }

    fn determine_advantage_first_cycle(&mut self, operation_player: Side) {
        match self.intelligence_condition {
            Intelligence::Surprise => {
                self.advantage_player = Some(operation_player);
            }
            Intelligence::Ambush | Intelligence::AmbushCV => {
                self.advantage_player = Some(operation_player.opposite());
            }
            Intelligence::Intercept => {
                self.determine_advantage_randomly(operation_player, 0, 0);
            }
        }
    }

    fn determine_advantage_other_cycle(&mut self, operation_player: Side) {
        match self.intelligence_condition {
            Intelligence::Surprise => self.determine_advantage_randomly(operation_player, 2, 0),
            Intelligence::Ambush | Intelligence::AmbushCV => {
                self.determine_advantage_randomly(operation_player, 0, 2)
            }
            Intelligence::Intercept => {
                self.determine_advantage_randomly(operation_player, 0, 0);
            }
        }
    }

    fn determine_advantage_randomly(
        &mut self,
        operation_player: Side,
        operation_bonus: u8,
        reaction_bonus: u8,
    ) {
        let mut rng = Rng::with_seed(self.seed);
        let operation_die = rng.u8(0..10);
        let reaction_die = rng.u8(0..10);

        if operation_die + operation_bonus >= reaction_die + reaction_bonus {
            self.advantage_player = Some(operation_player);
        } else {
            self.advantage_player = Some(operation_player.opposite());
        }
        self.seed = rng.get_seed();
    }

    pub fn movement(&mut self, movement: &MovementType) {
        match &mut self.phase {
            BattleCycleSegment::AdvantageMovement(phase) => {
                phase.remaining.retain(|m| m != movement);
                phase.current = Some(movement.clone());
            }
            BattleCycleSegment::DisadvantageMovement(phase) => {
                phase.remaining.retain(|m| m != movement);
                phase.current = Some(movement.clone());
            }
            _ => {}
        }
    }

    pub fn determine_naval_combat(
        &mut self,
        hex_type: &HexType,
        operation_detection: &Detection,
        reaction_detection: &Detection,
    ) {
        match &mut self.phase {
            BattleCycleSegment::NavalCombats(combat)
                if NavalCombat::should_skip_combat(
                    hex_type,
                    operation_detection,
                    reaction_detection,
                ) =>
            {
                self.phase = BattleCycleSegment::NavalCombats(NavalCombat::new(combat.index + 1));
            }
            BattleCycleSegment::NavalCombats(combat) => {
                combat.determine_surprise(
                    hex_type,
                    operation_detection,
                    reaction_detection,
                    &self.advantage_player,
                );
                combat.phase = NavalCombatPhase::NavalCombat1;
                combat.determine_distance(self.lighting_condition.as_ref().unwrap());
            }
            _ => {}
        }
    }

    pub fn bid_distance(
        &mut self,
        advantage_bid: &CombatDistance,
        disadvantage_bid: &CombatDistance,
    ) {
        match &mut self.phase {
            BattleCycleSegment::NavalCombats(combat) => {
                combat.bid_distance(advantage_bid, disadvantage_bid);
            }
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Lighting {
    DayAM,
    DayPM,
    Dusk,
    Night,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum BattleCycleSegment {
    SetLighting,
    AdvantageDetermination,
    AdvantageMovement(BattleMovementPhase),
    AdvantageAirMission,
    NavalCombats(NavalCombat),
    Bombardment,
    Demolition,
    GroundCombat,
    AirBaseRepair,
    Rally,
    DisadvantageMovement(BattleMovementPhase),
    DisadvantageAirMission,
    ActivationDeactivation,
    DetectionRemoval,
    DayAdjustment,
}

impl Display for BattleCycleSegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BattleCycleSegment::SetLighting => write!(f, "Set Lighting"),
            BattleCycleSegment::AdvantageDetermination => write!(f, "Advantage Determination"),
            BattleCycleSegment::AdvantageMovement(_) => write!(f, "Advantage Movement"),
            BattleCycleSegment::AdvantageAirMission => write!(f, "Advantage Air Mission"),
            BattleCycleSegment::NavalCombats(_) => write!(f, "Naval Combat"),
            BattleCycleSegment::Bombardment => write!(f, "Bombardment"),
            BattleCycleSegment::Demolition => write!(f, "Demolition"),
            BattleCycleSegment::GroundCombat => write!(f, "Ground Combat"),
            BattleCycleSegment::AirBaseRepair => write!(f, "Air Base Repair"),
            BattleCycleSegment::Rally => write!(f, "Rally"),
            BattleCycleSegment::DisadvantageMovement(_) => write!(f, "Disadvantage Movement"),
            BattleCycleSegment::DisadvantageAirMission => write!(f, "Disadvantage Air Mission"),
            BattleCycleSegment::ActivationDeactivation => write!(f, "Activation Deactivation"),
            BattleCycleSegment::DetectionRemoval => write!(f, "Detection Removal"),
            BattleCycleSegment::DayAdjustment => write!(f, "Day Adjustment"),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct NavalCombat {
    pub index: u8,
    pub hex_type: Option<HexType>,
    pub surprise: Option<Side>,
    pub phase: NavalCombatPhase,
    pub distance: Option<CombatDistance>,
    pub previous_distance: Option<CombatDistance>,
}

impl NavalCombat {
    fn new(index: u8) -> NavalCombat {
        NavalCombat {
            index,
            hex_type: None,
            surprise: None,
            phase: NavalCombatPhase::NavalCombatDetermination,
            distance: None,
            previous_distance: None,
        }
    }

    fn should_skip_combat(
        hex_type: &HexType,
        operation_detection: &Detection,
        reaction_detection: &Detection,
    ) -> bool {
        hex_type == &HexType::Open
            && operation_detection == &Detection::Undetected
            && reaction_detection == &Detection::Undetected
    }

    fn determine_surprise(
        &mut self,
        hex_type: &HexType,
        operation_detection: &Detection,
        reaction_detection: &Detection,
        advantage_player: &Option<Side>,
    ) {
        self.hex_type = Some(hex_type.clone());
        self.surprise = match (operation_detection, reaction_detection) {
            (Detection::Undetected, Detection::Detected) => advantage_player.clone(),
            (Detection::Detected, Detection::Undetected) => advantage_player.map(|p| p.opposite()),
            _ => None,
        };
    }

    fn determine_distance(&mut self, lighting: &Lighting) {
        match (&self.hex_type, lighting) {
            (Some(HexType::Open), Night) => self.distance = Some(CombatDistance::Medium),
            (Some(HexType::Open), _) => self.distance = Some(CombatDistance::Long),
            (Some(HexType::Coastal), Night) => self.distance = Some(CombatDistance::Short),
            (Some(HexType::Coastal), _) => self.distance = Some(CombatDistance::Medium),
            (Some(HexType::Restricted), _) => self.distance = Some(CombatDistance::Short),
            _ => {}
        }
    }

    fn next_round(&mut self) {
        match self.phase {
            NavalCombatPhase::NavalCombat1 => {
                self.phase = NavalCombatPhase::NavalCombat2;
                self.previous_distance.clone_from(&self.distance);
                self.distance = None;
            }
            NavalCombatPhase::NavalCombat2 => {
                self.phase = NavalCombatPhase::NavalCombat3;
                self.previous_distance.clone_from(&self.distance);
                self.distance = None;
            }
            NavalCombatPhase::NavalCombat3 => self.reset(),
            _ => {}
        }
    }

    fn reset(&mut self) {
        self.hex_type = None;
        self.surprise = None;
        self.phase = NavalCombatPhase::NavalCombatDetermination;
        self.distance = None;
        self.index += 1;
    }

    fn bid_distance(&mut self, advantage_bid: &CombatDistance, disadvantage_bid: &CombatDistance) {
        match self.previous_distance {
            Some(Long) => {
                self.distance = match (advantage_bid, disadvantage_bid) {
                    (Withdraw, Withdraw) => Some(Withdraw),
                    (Withdraw, _) => Some(Long),
                    (_, Withdraw) => Some(Long),
                    (Long, Long) => Some(Long),
                    (Long, Medium) => Some(Long),
                    (Short, Long) => Some(Medium),
                    (Long, Short) => Some(Medium),
                    (Medium, Long) => Some(Long),
                    (Medium, Medium) => Some(Medium),
                    (Medium, Short) => Some(Short),
                    (Short, Medium) => Some(Short),
                    (Short, Short) => Some(Short),
                }
            }
            Some(Medium) => {
                self.distance = match (advantage_bid, disadvantage_bid) {
                    (Withdraw, Withdraw) => Some(Withdraw),
                    (Withdraw, _) => Some(Medium),
                    (_, Withdraw) => Some(Medium),
                    (Long, Long) => Some(Long),
                    (Long, Medium) => Some(Long),
                    (Long, Short) => Some(Short),
                    (Short, Long) => Some(Short),
                    (Medium, Long) => Some(Long),
                    (Medium, Medium) => Some(Medium),
                    (Medium, Short) => Some(Short),
                    (Short, Medium) => Some(Short),
                    (Short, Short) => Some(Short),
                }
            }
            Some(Short) => {
                self.distance = match (advantage_bid, disadvantage_bid) {
                    (Withdraw, Withdraw) => Some(Withdraw),
                    (Withdraw, _) => Some(Short),
                    (_, Withdraw) => Some(Short),
                    (Long, Long) => Some(Medium),
                    (Long, Medium) => Some(Medium),
                    (Long, Short) => Some(Short),
                    (Short, Long) => Some(Short),
                    (Medium, Long) => Some(Medium),
                    (Medium, Medium) => Some(Medium),
                    (Medium, Short) => Some(Short),
                    (Short, Medium) => Some(Short),
                    (Short, Short) => Some(Short),
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum HexType {
    Restricted,
    Coastal,
    Open,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum NavalCombatPhase {
    NavalCombatDetermination,
    NavalCombat1,
    NavalCombat2,
    NavalCombat3,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum CombatDistance {
    Long,
    Medium,
    Short,
    Withdraw,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct BattleMovementPhase {
    pub remaining: Vec<MovementType>,
    pub current: Option<MovementType>,
}

impl BattleMovementPhase {
    fn new() -> Self {
        BattleMovementPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: None,
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum Detection {
    Detected,
    Undetected,
}

pub fn update_contact_phase(phase: &mut ContactPhase, movement: &MovementType) -> bool {
    match movement {
        NavalMovement => {
            if let Some(NavalMovement) = phase.current {
                phase.naval_movement_count += 1;
                if phase.naval_movement_count == phase.max_naval_movement_count {
                    phase.remaining.retain(|m| m != &NavalMovement);
                };
                (phase.naval_movement_count - 1) % 3 == 0
            } else {
                phase.current = Some(movement.clone());
                false
            }
        }
        other => {
            if let Some(NavalMovement) = phase.current {
                phase.remaining.retain(|m| m != &NavalMovement);
            }
            phase.current = Some(movement.clone());
            phase.remaining.retain(|m| m != other);
            false
        }
    }
}

#[cfg(test)]
pub mod core_test {
    use super::*;
    use super::{BattleCycleSegment::*, CombatDistance::*, Lighting::*, NewBattle, Side::*};
    use chrono::NaiveDate;

    pub fn coral_sea() -> NewBattle {
        NewBattle {
            battle_name: "Coral Sea".to_string(),
            start_date: NaiveDateForm {
                date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            },
            duration: 21,
            operation_player: Japan,
            intelligence_condition: Intelligence::Intercept,
        }
    }

    pub fn coral_sea_battle(id: Uuid) -> Battle {
        Battle {
            id,
            battle_data: coral_sea(),
            current_date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            phase: Phase::OperationContactPhase(ContactPhase::new()),
        }
    }

    #[test]
    fn choosing_naval_movement_at_contact_phase_keeps_it_from_available_movements() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: None,
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &NavalMovement);

        assert!(contact_phase.remaining.contains(&NavalMovement));
    }

    #[test]
    fn choosing_naval_movement_given_other_movement_type_does_not_increase_movement_count() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: None,
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &NavalMovement);

        assert_eq!(0, contact_phase.naval_movement_count);
    }

    #[test]
    fn choosing_naval_movement_given_maximum_movement_count_is_reached_removes_it_from_remaining() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: 3,
            naval_movement_count: 2,
        };

        update_contact_phase(&mut contact_phase, &NavalMovement);

        assert!(!contact_phase.remaining.contains(&NavalMovement));
    }

    #[test]
    fn choosing_ground_movement_given_naval_movement_is_in_play_removes_it_from_remaining() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &GroundMovement);

        assert_eq!(contact_phase.remaining, vec![]);
    }

    #[test]
    fn more_naval_movement_at_contact_phase_increases_movement_count() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &NavalMovement);

        assert_eq!(1, contact_phase.naval_movement_count);
    }

    #[test]
    fn naval_movement_increase_date_by_1_day_when_total_is_1() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        let add_day = update_contact_phase(&mut contact_phase, &NavalMovement);

        assert!(add_day);
    }

    #[test]
    fn naval_movement_does_not_increase_date_when_total_is_2() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 1,
        };

        let add_day = update_contact_phase(&mut contact_phase, &NavalMovement);

        assert!(!add_day);
    }

    #[test]
    fn naval_movement_increase_date_by_1_when_total_is_4() {
        let mut contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 3,
        };

        let add_day = update_contact_phase(&mut contact_phase, &NavalMovement);

        assert!(add_day);
    }

    #[test]
    fn battle_date_changes_when_naval_movement_increases_day() {
        let id = Uuid::new_v4();
        let contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 3,
        };

        let mut battle = Battle {
            id,
            battle_data: coral_sea(),
            current_date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            phase: Phase::OperationContactPhase(contact_phase),
        };

        battle.contact_movement(&NavalMovement);

        assert_eq!(
            NaiveDate::from_ymd_opt(1942, 05, 02).unwrap(),
            battle.current_date
        );
    }

    #[test]
    fn set_reaction_player_max_naval_movement_to_movement_count_from_operation_player() {
        let id = Uuid::new_v4();
        let contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 3,
        };

        let mut battle = Battle {
            id,
            battle_data: coral_sea(),
            current_date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            phase: Phase::OperationContactPhase(contact_phase),
        };

        battle.next();

        assert_eq!(
            3,
            if let Phase::ReactionContactPhase(phase) = battle.phase {
                phase.max_naval_movement_count
            } else {
                panic!("Expected ReactionContactPhase")
            }
        );
    }

    #[test]
    fn set_reaction_player_max_naval_movement_to_twice_count_from_operation_player_given_intelligence_is_ambush(
    ) {
        let id = Uuid::new_v4();
        let contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 3,
        };

        let mut battle = Battle {
            id,
            battle_data: NewBattle {
                battle_name: "Coral Sea".to_string(),
                start_date: NaiveDateForm {
                    date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
                },
                duration: 21,
                operation_player: Japan,
                intelligence_condition: Intelligence::Ambush,
            },
            current_date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            phase: Phase::OperationContactPhase(contact_phase),
        };

        battle.next();

        assert_eq!(
            6,
            if let Phase::ReactionContactPhase(phase) = battle.phase {
                phase.max_naval_movement_count
            } else {
                panic!("Expected ReactionContactPhase")
            }
        );
    }

    #[test]
    fn given_reaction_phase_when_next_move_to_battle_cycle() {
        let id = Uuid::new_v4();
        let contact_phase = ContactPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: Some(NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 3,
        };

        let mut battle = Battle {
            id,
            battle_data: coral_sea(),
            current_date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            phase: Phase::ReactionContactPhase(contact_phase),
        };

        battle.next();

        assert_eq!(
            Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128())),
            battle.phase
        );
    }

    #[test]
    fn given_battle_cycle_day_adjustment_when_next_moves_to_battle_cycle_start_and_add_2_days() {
        let id = Uuid::new_v4();
        let mut battle = Battle {
            id,
            battle_data: coral_sea(),
            current_date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            phase: Phase::BattleCyclePhase(BattleCycle {
                phase: DayAdjustment,
                ..BattleCycle::intercept(id.as_u128())
            }),
        };

        battle.next();

        if let Phase::BattleCyclePhase(cycle) = battle.phase {
            assert_eq!(SetLighting, cycle.phase);
            assert_eq!(2, cycle.count);
            assert_eq!(
                NaiveDate::from_ymd_opt(1942, 5, 3).unwrap(),
                battle.current_date
            );
        } else {
            panic!("Expected BattleCycle")
        }
    }

    #[test]
    fn lighting_can_be_set_by_operational_player_on_first_cycle() {
        let mut battle_cycle = BattleCycle::intercept(12);

        battle_cycle.choose_lighting(Dusk);

        assert_eq!(Some(Dusk), battle_cycle.lighting_condition);
    }

    #[test]
    fn lighting_can_advance_2_steps_by_operational_player_after_first_cycle() {
        let mut battle_cycle = BattleCycle::intercept(12);
        battle_cycle.next_lighting();
        battle_cycle.count = 2;

        assert!(battle_cycle.can_operation_player_advance_lighting());
    }

    #[test]
    fn lighting_can_advance_2_steps_by_operational_player_only_once() {
        let mut battle_cycle = BattleCycle::intercept(12);
        battle_cycle.next_lighting();
        battle_cycle.count = 2;

        battle_cycle.operation_advance_lighting();

        assert!(!battle_cycle.can_operation_player_advance_lighting());
    }

    #[test]
    fn operational_player_cannot_advance_lightning_if_already_chosen() {
        let mut battle_cycle = BattleCycle::intercept(12);
        battle_cycle.choose_lighting(Dusk);

        battle_cycle.count = 2;

        assert!(!battle_cycle.can_operation_player_advance_lighting());
    }

    #[test]
    fn lighting_can_be_chosen_randomly() {
        let mut battle_cycle = BattleCycle::intercept(12);

        battle_cycle.random_lighting();

        assert_eq!(Some(DayPM), battle_cycle.lighting_condition);
    }

    #[test]
    fn lighting_advances_one_step() {
        let mut battle_cycle = BattleCycle::intercept(12);
        battle_cycle.lighting_condition = Some(DayPM);

        battle_cycle.next_lighting();
        assert_eq!(Some(Dusk), battle_cycle.lighting_condition);

        battle_cycle.next_lighting();
        assert_eq!(Some(Night), battle_cycle.lighting_condition);

        battle_cycle.next_lighting();
        assert_eq!(Some(DayAM), battle_cycle.lighting_condition);
    }

    #[test]
    fn lighting_after_day_am_is_random() {
        let mut battle_cycle = BattleCycle::intercept(14);
        battle_cycle.lighting_condition = Some(DayAM);

        battle_cycle.next_lighting();

        assert_eq!(Some(Night), battle_cycle.lighting_condition);
    }

    #[test]
    fn lighting_next_is_random_given_its_not_set() {
        let mut battle_cycle = BattleCycle::intercept(14);

        battle_cycle.next_lighting();

        assert_eq!(Some(Night), battle_cycle.lighting_condition);
    }

    #[test]
    fn operation_player_has_advantage_on_1st_cycle_given_intelligence_is_surprise() {
        let mut battle_cycle = BattleCycle::intercept(14);
        battle_cycle.intelligence_condition = Intelligence::Surprise;

        battle_cycle.determine_advantage(Allies);

        assert_eq!(Some(Allies), battle_cycle.advantage_player);
    }

    #[test]
    fn reaction_player_has_advantage_on_1st_cycle_given_intelligence_is_ambush() {
        let mut battle_cycle = BattleCycle::intercept(14);
        battle_cycle.intelligence_condition = Intelligence::Ambush;

        battle_cycle.determine_advantage(Allies);

        assert_eq!(Some(Japan), battle_cycle.advantage_player);
    }

    #[test]
    fn advantage_is_decided_randomly_given_intelligence_is_intercept() {
        let mut battle_cycle = BattleCycle::intercept(16); // dice = 8 6

        battle_cycle.determine_advantage(Allies);

        assert_eq!(Some(Allies), battle_cycle.advantage_player);

        battle_cycle.seed = 14; // dice = 0 2
        battle_cycle.determine_advantage(Allies);

        assert_eq!(Some(Japan), battle_cycle.advantage_player);
    }

    #[test]
    fn advantage_roll_ties_resolve_to_operation_player() {
        let mut battle_cycle = BattleCycle::intercept(67); // dice = 3 3
        battle_cycle.determine_advantage(Allies);

        assert_eq!(Some(Allies), battle_cycle.advantage_player);
    }

    #[test]
    fn advantage_is_decided_randomly_after_1st_cycle_whatever_intelligence_level_is() {
        let mut battle_cycle = BattleCycle::intercept(10); // dice = 8 3
        battle_cycle.intelligence_condition = Intelligence::Ambush;
        battle_cycle.count = 2;

        battle_cycle.determine_advantage(Allies);

        assert_eq!(Some(Allies), battle_cycle.advantage_player);
    }

    #[test]
    fn operation_has_a_bonus_to_advantage_roll_given_intelligence_is_surprise() {
        let mut battle_cycle = BattleCycle::intercept(19); // dice = 2 4
        battle_cycle.intelligence_condition = Intelligence::Surprise;
        battle_cycle.count = 2;

        battle_cycle.determine_advantage(Allies);

        assert_eq!(Some(Allies), battle_cycle.advantage_player);
    }

    #[test]
    fn reaction_has_a_bonus_to_advantage_roll_given_intelligence_is_ambush() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.intelligence_condition = Intelligence::AmbushCV;
        battle_cycle.count = 2;

        battle_cycle.determine_advantage(Japan);

        assert_eq!(Some(Allies), battle_cycle.advantage_player);
    }

    #[test]
    fn advantage_movement_removes_chosen_type_from_remaining() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.intelligence_condition = Intelligence::AmbushCV;
        battle_cycle.count = 2;
        battle_cycle.next().next();

        battle_cycle.movement(&NavalMovement);

        if let AdvantageMovement(phase) = battle_cycle.phase {
            assert_eq!(vec![GroundMovement], phase.remaining);
            assert_eq!(NavalMovement, phase.current.unwrap());
        } else {
            panic!("Expected AdvantageMovement")
        }
    }

    #[test]
    fn disadvantage_movement_removes_chosen_type_from_remaining() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.intelligence_condition = Intelligence::AmbushCV;
        battle_cycle.count = 2;
        battle_cycle.phase = DisadvantageMovement(BattleMovementPhase {
            remaining: vec![GroundMovement, NavalMovement],
            current: None,
        });

        battle_cycle.movement(&NavalMovement);

        if let DisadvantageMovement(phase) = battle_cycle.phase {
            assert_eq!(vec![GroundMovement], phase.remaining);
            assert_eq!(NavalMovement, phase.current.unwrap());
        } else {
            panic!("Expected AdvantageMovement")
        }
    }

    #[test]
    fn naval_combat_takes_place_without_surprise_given_both_are_detected() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.intelligence_condition = Intelligence::AmbushCV;
        battle_cycle.count = 2;
        battle_cycle.phase = NavalCombats(NavalCombat::new(1));
        battle_cycle.lighting_condition = Some(DayAM);

        battle_cycle.determine_naval_combat(
            &HexType::Open,
            &Detection::Detected,
            &Detection::Detected,
        );

        if let NavalCombats(combat) = battle_cycle.phase {
            assert_eq!(Some(HexType::Open), combat.hex_type);
            assert_eq!(None, combat.surprise);
            assert_eq!(NavalCombatPhase::NavalCombat1, combat.phase);
            assert_eq!(Some(Long), combat.distance);
        } else {
            panic!("Expected NavalCombat")
        }
    }

    #[test]
    fn naval_combat_takes_place_with_surprise_given_advantage_is_undetected() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.intelligence_condition = Intelligence::AmbushCV;
        battle_cycle.count = 2;
        battle_cycle.phase = NavalCombats(NavalCombat::new(1));
        battle_cycle.advantage_player = Some(Japan);
        battle_cycle.lighting_condition = Some(DayAM);

        battle_cycle.determine_naval_combat(
            &HexType::Open,
            &Detection::Undetected,
            &Detection::Detected,
        );

        if let NavalCombats(combat) = battle_cycle.phase {
            assert_eq!(Some(HexType::Open), combat.hex_type);
            assert_eq!(Some(Japan), combat.surprise);
        } else {
            panic!("Expected NavalCombat")
        }
    }

    #[test]
    fn naval_combat_takes_place_with_surprise_given_reaction_is_undetected() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.intelligence_condition = Intelligence::AmbushCV;
        battle_cycle.count = 2;
        battle_cycle.phase = NavalCombats(NavalCombat::new(1));
        battle_cycle.advantage_player = Some(Japan);
        battle_cycle.lighting_condition = Some(DayAM);

        battle_cycle.determine_naval_combat(
            &HexType::Open,
            &Detection::Detected,
            &Detection::Undetected,
        );

        if let NavalCombats(combat) = battle_cycle.phase {
            assert_eq!(Some(HexType::Open), combat.hex_type);
            assert_eq!(Some(Allies), combat.surprise);
        } else {
            panic!("Expected NavalCombat")
        }
    }

    #[test]
    fn combat_does_not_take_place_given_both_are_undetected_in_open_water() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.intelligence_condition = Intelligence::AmbushCV;
        battle_cycle.count = 2;
        battle_cycle.phase = NavalCombats(NavalCombat::new(1));
        battle_cycle.advantage_player = Some(Japan);

        battle_cycle.determine_naval_combat(
            &HexType::Open,
            &Detection::Undetected,
            &Detection::Undetected,
        );

        if let NavalCombats(combat) = battle_cycle.phase {
            assert_eq!(2, combat.index);
        } else {
            panic!("Expected NavalCombat")
        }
    }

    macro_rules! first_round_distance_tests {
    ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (hex_type, lighting, expected_distance) = $value;
                let mut combat =  NavalCombat::new(1);
                combat.hex_type = Some(hex_type);

                combat.determine_distance(&lighting);

                assert_eq!(Some(expected_distance), combat.distance);
            }
        )*
      }
    }

    first_round_distance_tests! {
        first_round_distance_in_open_day_am: (HexType::Open, DayAM, Long),
        first_round_distance_in_open_day_pm: (HexType::Open, DayPM, Long),
        first_round_distance_in_open_dusk: (HexType::Open, Dusk, Long),
        first_round_distance_in_open_night: (HexType::Open, Night, Medium),
        first_round_distance_in_coastal_day_am: (HexType::Coastal, DayAM, Medium),
        first_round_distance_in_coastal_day_pm: (HexType::Coastal, DayPM, Medium),
        first_round_distance_in_coastal_dusk: (HexType::Coastal, Dusk, Medium),
        first_round_distance_in_coastal_night: (HexType::Coastal, Night, Short),
        first_round_distance_in_restricted_day_am: (HexType::Restricted, DayAM, Short),
        first_round_distance_in_restricted_day_pm: (HexType::Restricted, DayPM, Short),
        first_round_distance_in_restricted_dusk: (HexType::Restricted, Dusk, Short),
        first_round_distance_in_restricted_night: (HexType::Restricted, Night, Short),
    }

    macro_rules! bid_distance_tests {
    ($($name:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $name() {
                let (previous_distance, advantage_bid, disadvantage_bid, expected_distance) = $value;
                let mut combat =  NavalCombat::new(1);
                combat.phase = NavalCombatPhase::NavalCombat2;
                combat.previous_distance = Some(previous_distance);

                combat.bid_distance(&advantage_bid, &disadvantage_bid);

                assert_eq!(Some(expected_distance), combat.distance);

            }
        )*
      }
    }

    bid_distance_tests! {
            bid_distance_long_1: (Long, Withdraw, Withdraw, Withdraw),
            bid_distance_long_2: (Long, Withdraw, Long, Long),
            bid_distance_long_3: (Long,  Long, Withdraw, Long),
            bid_distance_long_4: (Long,  Long, Medium, Long),
            bid_distance_long_5: (Long,  Medium, Long, Long),
            bid_distance_long_6: (Long,  Long, Short, Medium),
            bid_distance_long_7: (Long,  Short, Long, Medium),
            bid_distance_long_8: (Long,  Medium, Medium, Medium),
            bid_distance_long_9: (Long,  Medium, Short, Short),
            bid_distance_long_10: (Long,  Short, Medium, Short),
            bid_distance_long_11: (Long,  Short, Short, Short),

            bid_distance_medium_1: (Medium, Withdraw, Withdraw, Withdraw),
            bid_distance_medium_2: (Medium, Withdraw, Long, Medium),
            bid_distance_medium_3: (Medium, Medium, Withdraw, Medium),
            bid_distance_medium_4: (Medium, Long, Long, Long),
            bid_distance_medium_5: (Medium, Medium, Long, Long),
            bid_distance_medium_6: (Medium, Long, Medium, Long),
            bid_distance_medium_7: (Medium, Long, Short, Short),
            bid_distance_medium_8: (Medium, Short, Long, Short),
            bid_distance_medium_9: (Medium, Medium, Medium, Medium),
            bid_distance_medium_10: (Medium, Medium, Short, Short),
            bid_distance_medium_11:(Medium, Short, Medium, Short),
            bid_distance_medium_12:(Medium, Short, Short, Short),

            bid_distance_short_1: (Short, Withdraw, Withdraw, Withdraw),
            bid_distance_short_2: (Short, Withdraw, Long, Short),
            bid_distance_short_3: (Short, Medium, Withdraw, Short),
            bid_distance_short_4: (Short, Long, Long, Medium),
            bid_distance_short_5: (Short, Medium, Long, Medium),
            bid_distance_short_6: (Short, Long, Medium, Medium),
            bid_distance_short_7: (Short, Long, Short, Short),
            bid_distance_short_8: (Short, Short, Long, Short),
            bid_distance_short_9: (Short, Medium, Medium, Medium),
            bid_distance_short_10:(Short, Medium, Short, Short),
            bid_distance_short_11:(Short, Short, Medium, Short),
            bid_distance_short_12:(Short, Short, Short, Short),
    }

    #[test]
    fn reset_naval_combat_after_round_3() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.phase = NavalCombats(NavalCombat::new(1));
        battle_cycle.lighting_condition = Some(DayAM);

        battle_cycle.determine_naval_combat(
            &HexType::Open,
            &Detection::Detected,
            &Detection::Detected,
        );

        battle_cycle.next_round().next_round().next_round();

        if let NavalCombats(combat) = battle_cycle.phase {
            assert_eq!(2, combat.index);
        } else {
            panic!("Expected NavalCombat")
        }
    }

    #[test]
    fn resets_naval_combat_distance_after_round_1() {
        let mut battle_cycle = BattleCycle::intercept(1); // dice = 7 6
        battle_cycle.phase = NavalCombats(NavalCombat::new(1));
        battle_cycle.lighting_condition = Some(DayAM);

        battle_cycle.determine_naval_combat(
            &HexType::Open,
            &Detection::Detected,
            &Detection::Detected,
        );

        battle_cycle.next_round();

        if let NavalCombats(combat) = battle_cycle.phase {
            assert_eq!(None, combat.distance);
            assert_eq!(Some(Long), combat.previous_distance);
        } else {
            panic!("Expected NavalCombat")
        }
    }
}

// dice 0: 5 0
// dice 1: 7 6
// dice 2: 2 4
// dice 3: 9 9
// dice 4: 4 7
// dice 5: 6 7
// dice 6: 3 2
// dice 7: 4 2
// dice 8: 0 0
// dice 9: 2 5
// dice 10: 8 3
// dice 11: 7 9
// dice 12: 3 6
// dice 13: 5 2
// dice 14: 0 2
// dice 15: 3 8
// dice 16: 8 6
// dice 17: 1 1
// dice 18: 7 9
// dice 19: 2 4
// dice 20: 8 2
// dice 21: 0 2
// dice 22: 6 7
// dice 23: 8 7
// dice 24: 4 5
// dice 25: 7 0
// dice 26: 2 8
// dice 27: 9 3
// dice 28: 5 1
// dice 29: 8 6
// dice 30: 3 7
// dice 31: 5 3
// dice 32: 0 0
// dice 33: 3 6
// dice 34: 8 3
// dice 35: 5 9
// dice 36: 1 7
// dice 37: 2 7
// dice 38: 9 1
// dice 39: 0 2
// dice 40: 6 0
// dice 41: 9 6
// dice 42: 4 3
// dice 43: 3 9
// dice 44: 9 6
// dice 45: 1 7
// dice 46: 6 2
// dice 47: 9 8
// dice 48: 5 5
// dice 49: 7 1
// dice 50: 3 8
// dice 51: 8 4
// dice 52: 4 1
// dice 53: 7 2
// dice 54: 2 1
// dice 55: 5 7
// dice 56: 0 4
// dice 57: 3 5
// dice 58: 9 2
// dice 59: 7 9
// dice 60: 3 5
// dice 61: 6 2
// dice 62: 1 6
// dice 63: 4 7
// dice 64: 9 5
// dice 65: 2 0
// dice 66: 8 8
// dice 67: 3 3
// dice 68: 8 1
// dice 69: 1 6
// dice 70: 7 7
// dice 71: 9 8
// dice 72: 5 5
// dice 73: 8 1
// dice 74: 3 8
// dice 75: 2 4
// dice 76: 7 1
// dice 77: 0 7
// dice 78: 6 1
// dice 79: 7 3
// dice 80: 4 0
// dice 81: 5 6
// dice 82: 1 4
// dice 83: 8 9
// dice 84: 3 7
// dice 85: 6 8
// dice 86: 1 2
// dice 87: 3 2
// dice 88: 8 0
// dice 89: 1 5
// dice 90: 6 3
// dice 91: 2 9
// dice 92: 8 6
// dice 93: 0 2
// dice 94: 6 3
// dice 95: 8 8
// dice 96: 3 6
// dice 97: 6 1
// dice 98: 2 9
// dice 99: 8 4
