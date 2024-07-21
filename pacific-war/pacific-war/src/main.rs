use chrono::NaiveDate;
use rocket::form;
use rocket::form::{Form, FromFormField, ValueField};
use rocket::http::impl_from_uri_param_identity;
use rocket::http::uri::fmt::{Formatter, Path, UriDisplay};
use rocket::http::Status;
use rocket::request::FromParam;
use rocket::response::Redirect;
use rocket::Build;
use rocket::Rocket;
use rocket::State;
use rocket_dyn_templates::context;
use rocket_dyn_templates::Template;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
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
            phase: Phase::OperationContactPhase(ContactPhase {
                remaining: vec![MovementType::GroundMovement, MovementType::AirMovemement],
                current: None,
            }),
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
        Some(battle) => {
            let start_date = battle
                .battle_data
                .start_date
                .date
                .format("%Y-%m-%d")
                .to_string();
            let current_date = battle.current_date.format("%Y-%m-%d").to_string();
            Ok(Template::render(
                "battle",
                context! {
                    battle_id: id.0,
                    battle_name : battle.battle_data.battle_name.clone(),
                    start_date,
                    current_date,
                    duration: format!("{}", battle.battle_data.duration),
                    operation_player: battle.battle_data.operation_player,
                    current_phase: format!("{}", battle.phase)
                },
            ))
        }
    }
}

#[derive(Debug, PartialEq)]
struct Battle {
    id: Uuid,
    battle_data: NewBattle,
    current_date: NaiveDate,
    phase: Phase,
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
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
enum MovementType {
    GroundMovement,
    AirMovemement,
    NavalMovement(u8), // number of hexes moved
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
        .mount("/", routes![index, create_battle, battle])
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
                phase: Phase::OperationContactPhase(ContactPhase {
                    remaining: vec![MovementType::GroundMovement, MovementType::AirMovemement],
                    current: None,
                }),
            },
        );
        let battles = Arc::new(Mutex::new(battles_map));
        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client.get(format!("/battle/{}", id)).dispatch();
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("Current date: 1942-05-01"));
        assert!(response_string.contains("21"));
        assert!(response_string.contains("Operation Contact Phase"));
    }
}
