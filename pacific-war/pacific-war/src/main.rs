use chrono::NaiveDate;
use rocket::form;
use rocket::form::{Form, FromFormField, ValueField};
use rocket::fs::FileServer;
use rocket::http::impl_from_uri_param_identity;
use rocket::http::uri::fmt::{Formatter, Path, UriDisplay};
use rocket::http::Status;
use rocket::request::FromParam;
use rocket::response::Redirect;
use rocket::Build;
use rocket::Rocket;
use rocket::State;
use rocket_dyn_templates::context;
use rocket_dyn_templates::handlebars;
use rocket_dyn_templates::Template;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;
use uuid::Uuid;

#[macro_use]
extern crate rocket;

#[get("/")]
fn index() -> Template {
    Template::render("index", context! {})
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, Copy)]
struct NaiveDateForm {
    date: NaiveDate,
}

#[rocket::async_trait]
impl<'v> FromFormField<'v> for NaiveDateForm {
    fn from_value(form_value: ValueField<'v>) -> form::Result<'v, Self> {
        Ok(NaiveDate::parse_from_str(&form_value.value, "%Y-%m-%d")
            .map(|date| NaiveDateForm { date })
            .map_err(|_| form::Error::validation("not a date"))?)
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, FromFormField, Clone, Copy)]
enum Side {
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

#[derive(Debug, PartialEq, Serialize, Deserialize, FromForm, Clone)]
struct NewBattle {
    battle_name: String,
    start_date: NaiveDateForm,
    duration: u8,
    operation_player: Side,
}

impl NewBattle {
    fn to_form(&self) -> String {
        format!(
            "battle_name={}&start_date={}&duration={}&operation_player={}",
            self.battle_name, self.start_date.date, self.duration, self.operation_player
        )
    }
}

#[post("/battle", data = "<form>")]
fn create_battle(battles: &State<Battles>, form: Form<NewBattle>) -> Redirect {
    let new_battle = form.into_inner();
    let uuid = Uuid::new_v4();
    battles.battles.lock().unwrap().insert(
        uuid,
        Battle {
            id: uuid,
            battle_data: new_battle.clone(),
            current_date: new_battle.start_date.date.clone(),
            phase: Phase::OperationContactPhase(ContactPhase::new()),
        },
    );
    Redirect::to(uri!(battle(id = UuidForm(uuid))))
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct UuidForm(Uuid);

impl UriDisplay<Path> for UuidForm {
    fn fmt(&self, f: &mut Formatter<Path>) -> fmt::Result {
        f.write_raw(self.0.to_string())?;
        Ok(())
    }
}

impl_from_uri_param_identity!([Path] UuidForm);

#[rocket::async_trait]
impl<'v> FromParam<'v> for UuidForm {
    type Error = &'v str;

    fn from_param(param: &'v str) -> Result<Self, Self::Error> {
        Uuid::parse_str(param)
            .map(|uuid| UuidForm(uuid))
            .map_err(|_| "not a valid uuid")
    }
}

/// Renders a battle with given id
#[get("/battle/<id>")]
fn battle(battles: &State<Battles>, id: UuidForm) -> Result<Template, Status> {
    let battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get(&id.0);
    match battle {
        None => Err(Status::NotFound),
        Some(battle) => match battle.phase {
            Phase::OperationContactPhase(_) => contact_phase(battle),
            Phase::ReactionContactPhase(_) => contact_phase(battle),
            Phase::BattleCycle(_) => todo!(),
        },
    }
}

/// Update current phase
#[post("/battle/<id>/<movement>")]
fn contact_movement(
    battles: &State<Battles>,
    movement: MovementType,
    id: UuidForm,
) -> Result<Template, Status> {
    let mut battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get_mut(&id.0).ok_or(Status::NotFound)?;
    match &mut battle.phase {
        Phase::OperationContactPhase(_) => {
            battle.contact_movement(&movement);
            Ok(contact_phase(battle)?)
        }
        Phase::ReactionContactPhase(_) => {
            battle.contact_movement(&movement);
            Ok(contact_phase(battle)?)
        }
        _ => Err(Status::BadRequest),
    }
}

fn update_contact_phase(phase: &mut ContactPhase, movement: &MovementType) -> bool {
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

fn contact_phase(battle: &Battle) -> Result<Template, Status> {
    match &battle.phase {
        Phase::OperationContactPhase(phase) => Ok(render_contact_phase(battle, phase)),
        Phase::ReactionContactPhase(phase) => Ok(render_contact_phase(battle, phase)),
        _ => todo!(),
    }
}

fn render_contact_phase(battle: &Battle, phase: &ContactPhase) -> Template {
    Template::render(
        "contact",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            battle_name : &battle.battle_data.battle_name,
            start_date : &battle.battle_data.start_date.date,
            duration : &battle.battle_data.duration,
            current_date : &battle.current_date,
            phase_name : format!("{}", &battle.phase),
            parent: "battle",
            phase: &phase,
        },
    )
}

/// Update current phase
#[get("/battle/<id>/next")]
fn battle_next(battles: &State<Battles>, id: UuidForm) -> Result<Template, Status> {
    let mut battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get_mut(&id.0).ok_or(Status::NotFound)?;
    match &mut battle.phase {
        Phase::OperationContactPhase(_) => {
            battle.next();
            Ok(contact_phase(battle)?)
        }
        // Phase::ReactionContactPhase(_) => {
        //     battle.phase = Phase::BattleCycle(BattleCyclePhase::Lighting);
        //     Ok(battle_cycle_phase(battle)?)
        // }
        _ => Err(Status::BadRequest),
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
struct Battle {
    id: Uuid,
    battle_data: NewBattle,
    current_date: NaiveDate,
    phase: Phase,
}

impl Battle {
    fn contact_movement(&mut self, movement: &MovementType) {
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

    fn next(&mut self) {
        match &mut self.phase {
            Phase::OperationContactPhase(phase) => {
                self.phase = Phase::ReactionContactPhase(ContactPhase {
                    max_naval_movement_count: phase.naval_movement_count,
                    ..ContactPhase::new()
                });
            }
            Phase::ReactionContactPhase(_) => {
                self.phase = Phase::BattleCycle(BattleCyclePhase::Lighting);
            }
            _ => {}
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
enum Phase {
    OperationContactPhase(ContactPhase),
    ReactionContactPhase(ContactPhase),
    BattleCycle(BattleCyclePhase),
}

impl Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Phase::OperationContactPhase(_) => write!(f, "Operation Contact Phase"),
            Phase::ReactionContactPhase(_) => write!(f, "Reaction Contact Phase"),
            Phase::BattleCycle(_) => write!(f, "Battle Cycle"),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
struct ContactPhase {
    remaining: Vec<MovementType>,
    current: Option<MovementType>,
    /// Maximum number of naval movements allowed.
    ///
    /// Defaults to u8::MAX for Operation player, and then is set to
    /// whatever number of hexes operation player moved for reaction
    /// player
    max_naval_movement_count: u8,
    naval_movement_count: u8,
}

impl ContactPhase {
    fn new() -> Self {
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
enum MovementType {
    GroundMovement,
    AirMovement,
    NavalMovement,
}

impl<'v> FromParam<'v> for MovementType {
    type Error = &'v str;

    fn from_param(param: &'v str) -> Result<Self, Self::Error> {
        match param {
            "GroundMovement" => Ok(MovementType::GroundMovement),
            "AirMovement" => Ok(MovementType::AirMovement),
            "NavalMovement" => Ok(MovementType::NavalMovement),
            _ => Err("not a valid movement type"),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
enum BattleCyclePhase {
    Lighting,
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
enum NavalBattleCycle {
    NavalCombatDetermination,
    NavalCombat(u8),
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
struct BattleMovementPhase {
    remaining: Vec<MovementType>,
    current: Option<MovementType>,
}

struct Battles {
    battles: Arc<Mutex<HashMap<Uuid, Battle>>>,
}

#[launch]
fn rocket() -> _ {
    rocket_with_state(Arc::new(Mutex::new(HashMap::new())))
}

fn rocket_with_state(battles: Arc<Mutex<HashMap<Uuid, Battle>>>) -> Rocket<Build> {
    rocket::build()
        .manage(Battles { battles })
        .attach(Template::fairing())
        .mount(
            "/",
            routes![index, create_battle, battle, battle_next, contact_movement],
        )
        .mount("/public", FileServer::from("static"))
}

#[cfg(test)]
mod test {
    use super::*;
    use super::{NewBattle, Side};
    use chrono::NaiveDate;
    use rocket::http::ContentType;
    use rocket::http::Status;
    use rocket::local::blocking::Client;

    #[test]
    fn can_serve_css_file() {
        let client = Client::tracked(rocket()).expect("valid rocket instance");
        let response = client.get("/public/pacific.css").dispatch();
        assert_eq!(response.status(), Status::Ok);
    }

    #[test]
    fn post_battle_redirects_to_new_battle_with_uuid() {
        let client = Client::tracked(rocket()).expect("valid rocket instance");
        let battle = NewBattle {
            battle_name: "Coral Sea".to_string(),
            start_date: NaiveDateForm {
                date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
            },
            duration: 21,
            operation_player: Side::Japan,
        };
        let response = client
            .post("/battle")
            .header(ContentType::Form)
            .body(battle.to_form())
            .dispatch();
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[test]
    fn get_existing_battle_returns_template() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        battles_map.insert(
            id,
            Battle {
                id,
                battle_data: NewBattle {
                    battle_name: "Coral Sea".to_string(),
                    start_date: NaiveDateForm {
                        date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                    },
                    duration: 21,
                    operation_player: Side::Japan,
                },
                current_date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                phase: Phase::OperationContactPhase(ContactPhase::new()),
            },
        );
        let battles = Arc::new(Mutex::new(battles_map));
        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client.get(format!("/battle/{}", id)).dispatch();
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("japan"));
        assert!(response_string.contains("Current date: 1942-05-01"));
        assert!(response_string.contains("21"));
        assert!(response_string.contains("Operation Contact Phase"));
        assert!(response_string.contains("GroundMovement"));
        assert!(response_string.contains("AirMovement"));
        assert!(response_string.contains("NavalMovement"));
        assert!(response_string.contains("Next"));
    }

    #[test]
    fn choosing_ground_movement_at_contact_phase_removes_it_from_available_movements() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        battles_map.insert(
            id,
            Battle {
                id,
                battle_data: NewBattle {
                    battle_name: "Coral Sea".to_string(),
                    start_date: NaiveDateForm {
                        date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                    },
                    duration: 21,
                    operation_player: Side::Japan,
                },
                current_date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                phase: Phase::OperationContactPhase(ContactPhase::new()),
            },
        );
        let battles = Arc::new(Mutex::new(battles_map));
        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/GroundMovement", id))
            .dispatch();
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("Current date: 1942-05-01"));
        assert!(response_string.contains("21"));
        assert!(response_string.contains("Operation Contact Phase"));
        assert!(!response_string.contains(format!("{}/GroundMovement", id).as_str()));
        assert!(response_string.contains("AirMovement"));
        assert!(response_string.contains("NavalMovement"));
        assert!(response_string.contains("Next"));
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
        let mut contact_phase = ContactPhase {
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
                    date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                },
                duration: 21,
                operation_player: Side::Japan,
            },
            current_date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
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
        let mut contact_phase = ContactPhase {
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
                    date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                },
                duration: 21,
                operation_player: Side::Japan,
            },
            current_date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
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
    fn choosing_next_at_operation_contact_phase_moves_to_reaction_contact_phase() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        battles_map.insert(
            id,
            Battle {
                id,
                battle_data: NewBattle {
                    battle_name: "Coral Sea".to_string(),
                    start_date: NaiveDateForm {
                        date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                    },
                    duration: 21,
                    operation_player: Side::Japan,
                },
                current_date: NaiveDate::from_ymd_opt(1942, 05, 01).unwrap(),
                phase: Phase::OperationContactPhase(ContactPhase::new()),
            },
        );
        let battles = Arc::new(Mutex::new(battles_map));
        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client.get(format!("/battle/{}/next", id)).dispatch();
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        assert!(response_string.contains("Reaction Contact Phase"));
        assert!(response_string.contains("GroundMovement"));
        assert!(response_string.contains("AirMovement"));
        assert!(response_string.contains("NavalMovement"));
        assert!(response_string.contains("Next"));
    }
}
