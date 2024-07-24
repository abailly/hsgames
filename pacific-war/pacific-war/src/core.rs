use chrono::NaiveDate;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::Display;
use uuid::Uuid;

use crate::util::*;

#[derive(Debug, PartialEq, Serialize, Deserialize, FromFormField, Clone, Copy)]
pub enum Side {
    Japan,
    Allies,
}

impl Display for Side {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Side::Japan => write!(f, "japan"),
            Side::Allies => write!(f, "allies"),
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
                self.phase = Phase::BattleCyclePhase(BattleCycle::new());
            }
            Phase::BattleCyclePhase(battle_cycle) => {
                if battle_cycle.next() {
                    self.current_date = self.current_date.succ().succ();
                }
            }
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
            remaining: vec![
                MovementType::GroundMovement,
                MovementType::AirMovement,
                MovementType::NavalMovement,
            ],
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
    pub count: u8,
    pub phase: BattleCycleSegment,
}

impl BattleCycle {
    pub fn new() -> Self {
        BattleCycle {
            lighting_condition: None,
            count: 1,
            phase: BattleCycleSegment::SetLighting,
        }
    }

    pub fn set_lighting(&mut self, lighting: Lighting) {
        self.lighting_condition = Some(lighting);
    }

    fn next(&mut self) -> bool {
        match self.phase {
            BattleCycleSegment::SetLighting => {
                self.phase = BattleCycleSegment::AdvantageDetermination;
            }
            BattleCycleSegment::AdvantageDetermination => {
                self.phase = BattleCycleSegment::AdvantageMovement(BattleMovementPhase {
                    remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
                    current: None,
                });
            }
            BattleCycleSegment::AdvantageMovement(_) => {
                self.phase = BattleCycleSegment::AdvantageAirMission;
            }
            BattleCycleSegment::AdvantageAirMission => {
                self.phase =
                    BattleCycleSegment::NavalCombat(NavalBattleCycle::NavalCombatDetermination);
            }
            BattleCycleSegment::NavalCombat(_) => {
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
                self.phase = BattleCycleSegment::DisadvantageMovement(BattleMovementPhase {
                    remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
                    current: None,
                });
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
                return true;
            }
        }
        false
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
    NavalCombat(NavalBattleCycle),
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

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub enum NavalBattleCycle {
    NavalCombatDetermination,
    NavalCombat(u8),
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct BattleMovementPhase {
    pub remaining: Vec<MovementType>,
    pub current: Option<MovementType>,
}

pub fn update_contact_phase(phase: &mut ContactPhase, movement: &MovementType) -> bool {
    match movement {
        MovementType::NavalMovement => {
            if let Some(MovementType::NavalMovement) = phase.current {
                phase.naval_movement_count += 1;
                if phase.naval_movement_count == phase.max_naval_movement_count {
                    phase
                        .remaining
                        .retain(|m| m != &MovementType::NavalMovement);
                };
                (phase.naval_movement_count - 1) % 3 == 0
            } else {
                phase.current = Some(movement.clone());
                false
            }
        }
        other => {
            if let Some(MovementType::NavalMovement) = phase.current {
                phase
                    .remaining
                    .retain(|m| m != &MovementType::NavalMovement);
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
    use super::{BattleCycleSegment::*, NewBattle, Side};
    use chrono::NaiveDate;

    pub fn coral_sea() -> NewBattle {
        NewBattle {
            battle_name: "Coral Sea".to_string(),
            start_date: NaiveDateForm {
                date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            },
            duration: 21,
            operation_player: Side::Japan,
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
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: None,
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &MovementType::NavalMovement);

        assert!(contact_phase
            .remaining
            .contains(&MovementType::NavalMovement));
    }

    #[test]
    fn choosing_naval_movement_given_other_movement_type_does_not_increase_movement_count() {
        let mut contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: None,
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &MovementType::NavalMovement);

        assert_eq!(0, contact_phase.naval_movement_count);
    }

    #[test]
    fn choosing_naval_movement_given_maximum_movement_count_is_reached_removes_it_from_remaining() {
        let mut contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
            max_naval_movement_count: 3,
            naval_movement_count: 2,
        };

        update_contact_phase(&mut contact_phase, &MovementType::NavalMovement);

        assert!(!contact_phase
            .remaining
            .contains(&MovementType::NavalMovement));
    }

    #[test]
    fn choosing_ground_movement_given_naval_movement_is_in_play_removes_it_from_remaining() {
        let mut contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &MovementType::GroundMovement);

        assert_eq!(contact_phase.remaining, vec![]);
    }

    #[test]
    fn more_naval_movement_at_contact_phase_increases_movement_count() {
        let mut contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        update_contact_phase(&mut contact_phase, &MovementType::NavalMovement);

        assert_eq!(1, contact_phase.naval_movement_count);
    }

    #[test]
    fn naval_movement_increase_date_by_1_day_when_total_is_1() {
        let mut contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 0,
        };

        let add_day = update_contact_phase(&mut contact_phase, &MovementType::NavalMovement);

        assert!(add_day);
    }

    #[test]
    fn naval_movement_does_not_increase_date_when_total_is_2() {
        let mut contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 1,
        };

        let add_day = update_contact_phase(&mut contact_phase, &MovementType::NavalMovement);

        assert!(!add_day);
    }

    #[test]
    fn naval_movement_increase_date_by_1_when_total_is_4() {
        let mut contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 3,
        };

        let add_day = update_contact_phase(&mut contact_phase, &MovementType::NavalMovement);

        assert!(add_day);
    }

    #[test]
    fn battle_date_changes_when_naval_movement_increases_day() {
        let id = Uuid::new_v4();
        let contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
            max_naval_movement_count: u8::MAX,
            naval_movement_count: 3,
        };

        let mut battle = Battle {
            id,
            battle_data: coral_sea(),
            current_date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            phase: Phase::OperationContactPhase(contact_phase),
        };

        battle.contact_movement(&MovementType::NavalMovement);

        assert_eq!(
            NaiveDate::from_ymd_opt(1942, 05, 02).unwrap(),
            battle.current_date
        );
    }

    #[test]
    fn set_reaction_player_max_naval_movement_to_movement_count_from_operation_player() {
        let id = Uuid::new_v4();
        let contact_phase = ContactPhase {
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
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
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
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
                operation_player: Side::Japan,
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
            remaining: vec![MovementType::GroundMovement, MovementType::NavalMovement],
            current: Some(MovementType::NavalMovement),
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

        assert_eq!(Phase::BattleCyclePhase(BattleCycle::new()), battle.phase);
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
                ..BattleCycle::new()
            }),
        };

        battle.next();

        if let Phase::BattleCyclePhase(cycle) = battle.phase {
            assert_eq!(SetLighting, cycle.phase);
            assert_eq!(2, cycle.count);
            assert_eq!(
                NaiveDate::from_ymd_opt(1942, 05, 03).unwrap(),
                battle.current_date
            );
        } else {
            panic!("Expected BattleCycle")
        }
    }

    #[test]
    fn lighting_can_be_set_by_operational_player_on_first_cycle() {
        let id = Uuid::new_v4();
        let mut battle_cycle = BattleCycle::new();

        battle_cycle.set_lighting(Lighting::Dusk);

        assert_eq!(Some(Lighting::Dusk), battle_cycle.lighting_condition);
    }
}
