use rocket::form;
use rocket::form::Form;
use rocket::form::{FromFormField, ValueField};
use rocket::fs::FileServer;
use rocket::http::Status;
use rocket::request::FromParam;
use rocket::response::Redirect;
use rocket::Build;
use rocket::Rocket;
use rocket::State;
use rocket_dyn_templates::context;
use rocket_dyn_templates::Template;
use std::collections::HashMap;
use std::convert::From;
use std::sync::Arc;
use std::sync::Mutex;
use uuid::Uuid;

mod util;
use util::*;

mod core;
use core::*;

#[macro_use]
extern crate rocket;

#[get("/")]
fn index() -> Template {
    Template::render("index", context! {})
}

#[post("/battle", data = "<form>")]
fn create_battle(battles: &State<Battles>, form: Form<NewBattle>) -> Redirect {
    let new_battle = form.into_inner();
    let uuid = Uuid::new_v4();
    battles
        .battles
        .lock()
        .unwrap()
        .insert(uuid, Battle::new(uuid, &new_battle));
    Redirect::to(uri!(battle(id = UuidForm::from(uuid))))
}

#[rocket::async_trait]
impl<'v> FromParam<'v> for UuidForm {
    type Error = &'v str;

    fn from_param(param: &'v str) -> Result<Self, Self::Error> {
        Uuid::parse_str(param)
            .map(|uuid| UuidForm { uuid })
            .map_err(|_| "not a valid uuid")
    }
}

/// Renders a battle with given id
#[get("/battle/<id>")]
fn battle(battles: &State<Battles>, id: UuidForm) -> Result<Template, Status> {
    let battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get(&id.uuid);
    match battle {
        None => Err(Status::NotFound),
        Some(battle) => contact_phase(battle),
    }
}

/// Update movement for contact phase
#[post("/battle/<id>/contact/<movement>")]
fn contact_movement(
    battles: &State<Battles>,
    movement: MovementType,
    id: UuidForm,
) -> Result<Template, Status> {
    let mut battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get_mut(&id.uuid).ok_or(Status::NotFound)?;
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

fn contact_phase(battle: &Battle) -> Result<Template, Status> {
    match &battle.phase {
        Phase::OperationContactPhase(phase) => Ok(render_contact_phase(battle, phase)),
        Phase::ReactionContactPhase(phase) => Ok(render_contact_phase(battle, phase)),
        Phase::BattleCyclePhase(phase) => Ok(render_battle_cycle(battle, phase)),
    }
}

fn render_contact_phase(battle: &Battle, phase: &ContactPhase) -> Template {
    Template::render(
        "contact",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
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

fn render_battle_cycle(battle: &Battle, cycle: &BattleCycle) -> Template {
    match &cycle.phase {
        BattleCycleSegment::SetLighting => render_battle_cycle_lighting(battle, cycle),
        BattleCycleSegment::AdvantageDetermination => {
            render_battle_cycle_advantage_determination(battle, cycle)
        }
        BattleCycleSegment::AdvantageMovement(phase) => {
            render_battle_cycle_movement(battle, cycle, phase)
        }
        BattleCycleSegment::DisadvantageMovement(phase) => {
            render_battle_cycle_movement(battle, cycle, phase)
        }
        _ => render_battle_cycle_default(battle, cycle),
        // BattleCyclePhase::NavalCombat(phase) => render_battle_cycle_naval_combat(battle, phase),
    }
}

fn render_battle_cycle_movement(
    battle: &Battle,
    cycle: &BattleCycle,
    phase: &BattleMovementPhase,
) -> Template {
    Template::render(
        "battle_cycle/movement",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
            battle_name : &battle.battle_data.battle_name,
            start_date : &battle.battle_data.start_date.date,
            duration : &battle.battle_data.duration,
            current_date : &battle.current_date,
            cycle: &cycle,
            cycle_phase: format!("{}", cycle.phase),
            movement: &phase,
            parent: "battle",
        },
    )
}

fn render_battle_cycle_advantage_determination(battle: &Battle, cycle: &BattleCycle) -> Template {
    Template::render(
        "battle_cycle/advantage_determination",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
            battle_name : &battle.battle_data.battle_name,
            start_date : &battle.battle_data.start_date.date,
            duration : &battle.battle_data.duration,
            current_date : &battle.current_date,
            cycle: &cycle,
            cycle_phase: format!("{}", cycle.phase),
            parent: "battle",
        },
    )
}

fn render_battle_cycle_lighting(battle: &Battle, cycle: &BattleCycle) -> Template {
    Template::render(
        "battle_cycle/lighting",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
            battle_name : &battle.battle_data.battle_name,
            start_date : &battle.battle_data.start_date.date,
            duration : &battle.battle_data.duration,
            current_date : &battle.current_date,
            cycle: &cycle,
            cycle_phase: format!("{}", cycle.phase),
            // FIXME: lighting selection logic is complicated
            choose: cycle.count == 1,
            reaction_choose: cycle.can_reaction_player_choose_lighting(),
            operation_advance: cycle.can_operation_player_advance_lighting(),
            parent: "battle",
        },
    )
}

fn render_battle_cycle_default(battle: &Battle, cycle: &BattleCycle) -> Template {
    Template::render(
        "battle_cycle/default",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
            battle_name : &battle.battle_data.battle_name,
            start_date : &battle.battle_data.start_date.date,
            duration : &battle.battle_data.duration,
            current_date : &battle.current_date,
            parent: "battle",
            cycle: &cycle,
            cycle_phase: format!("{}", cycle.phase),
        },
    )
}

/// Update current phase
#[get("/battle/<id>/next")]
fn battle_next(battles: &State<Battles>, id: UuidForm) -> Result<Template, Status> {
    let mut battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get_mut(&id.uuid).ok_or(Status::NotFound)?;
    battle.next();
    contact_phase(battle)
}

struct Battles {
    battles: Arc<Mutex<HashMap<Uuid, Battle>>>,
}

impl FromFormField<'_> for Lighting {
    fn from_value(field: ValueField<'_>) -> form::Result<'_, Self> {
        match field.value {
            "DayAM" => Ok(Lighting::DayAM),
            "DayPM" => Ok(Lighting::DayPM),
            "Dusk" => Ok(Lighting::Dusk),
            "Night" => Ok(Lighting::Night),
            v => Err(
                form::Error::validation(format!("not a valid lighting condition {:?}", v)).into(),
            ),
        }
    }
}

#[derive(FromForm)]
struct SetLightingForm<'r> {
    lighting: Option<Lighting>,
    r#set_lighting: &'r str,
}

#[post("/battle/<id>/set_lighting", data = "<form>")]
fn set_lighting(
    battles: &State<Battles>,
    id: UuidForm,
    form: Form<SetLightingForm>,
) -> Result<Redirect, Status> {
    let mut battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get_mut(&id.uuid).ok_or(Status::NotFound)?;
    match &mut battle.phase {
        Phase::BattleCyclePhase(phase) => {
            let inner = form.into_inner();
            match inner.r#set_lighting {
                "choose" => {
                    if let Some(lighting) = inner.lighting {
                        phase.choose_lighting(lighting);
                    } else {
                        return Err(Status::BadRequest);
                    }
                }
                "advance" => {
                    phase.next_lighting();
                }
                "operation_advance" => {
                    phase.operation_advance_lighting();
                }
                _ => {
                    return Err(Status::BadRequest);
                }
            }
            battle.next();
            // FIXME: would rather a return a template directly here but borrow checker
            // prevents this because we are borrowing `battle` mutably
            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
}

#[post("/battle/<id>/advantage_determination")]
fn advantage_determination(battles: &State<Battles>, id: UuidForm) -> Result<Redirect, Status> {
    let mut battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get_mut(&id.uuid).ok_or(Status::NotFound)?;
    match &mut battle.phase {
        Phase::BattleCyclePhase(_) => {
            battle.determine_advantage();
            battle.next();
            // FIXME: would rather a return a template directly here but borrow checker
            // prevents this because we are borrowing `battle` mutably
            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
}

#[derive(FromForm)]
struct MovementForm {
    movement: MovementType,
}

impl FromFormField<'_> for MovementType {
    fn from_value(field: ValueField<'_>) -> form::Result<'_, Self> {
        match field.value {
            "NavalMovement" => Ok(MovementType::NavalMovement),
            "GroundMovement" => Ok(MovementType::GroundMovement),
            "AirMovement" => Ok(MovementType::AirMovement),
            _ => Err(form::Error::validation(format!(
                "not a valid movement type {:?}",
                field.value
            ))
            .into()),
        }
    }
}

/// Update movement for battle phase
#[post("/battle/<id>/movement", data = "<form>")]
fn movement(
    battles: &State<Battles>,
    id: UuidForm,
    form: Form<MovementForm>,
) -> Result<Redirect, Status> {
    let mut battles_map = battles.battles.lock().unwrap();
    let battle = battles_map.get_mut(&id.uuid).ok_or(Status::NotFound)?;
    match &mut battle.phase {
        Phase::BattleCyclePhase(cycle) => {
            cycle.movement(&form.into_inner().movement);
            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
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
            routes![
                index,
                create_battle,
                battle,
                battle_next,
                contact_movement,
                set_lighting,
                advantage_determination,
                movement
            ],
        )
        .mount("/public", FileServer::from("static"))
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

#[cfg(test)]
mod test {
    use super::*;
    use core_test::{coral_sea, coral_sea_battle};
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
        let battle = coral_sea();
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
        battles_map.insert(id, coral_sea_battle(id));
        let battles = Arc::new(Mutex::new(battles_map));
        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client.get(format!("/battle/{}", id)).dispatch();
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("Intercept"));
        assert!(response_string.contains("Japan"));
        assert!(response_string.contains("value=\"1942-05-01\""));
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
        battles_map.insert(id, coral_sea_battle(id));
        let battles = Arc::new(Mutex::new(battles_map));
        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/contact/GroundMovement", id))
            .dispatch();
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("21"));
        assert!(response_string.contains("Operation Contact Phase"));
        assert!(!response_string.contains(format!("{}/GroundMovement", id).as_str()));
        assert!(response_string.contains("AirMovement"));
        assert!(response_string.contains("NavalMovement"));
        assert!(response_string.contains("Next"));
    }

    #[test]
    fn choosing_next_at_operation_contact_phase_moves_to_reaction_contact_phase() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        battles_map.insert(id, coral_sea_battle(id));
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

    #[test]
    fn can_choose_lighting_condition_when_battle_cycle_starts() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));
        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/set_lighting", id))
            .header(ContentType::Form)
            .body("lighting=Night&set_lighting=choose")
            .dispatch();
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[test]
    fn advance_lighting_condition() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));
        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/set_lighting", id))
            .header(ContentType::Form)
            .body("set_lighting=advance")
            .dispatch();
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[test]
    fn can_advance_lighting_condition_2_steps_only_once() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        let mut battle_cycle = BattleCycle::intercept(id.as_u128());
        battle_cycle.count = 3;
        battle_cycle.lighting_condition = Some(Lighting::DayPM);
        battle.phase = Phase::BattleCyclePhase(battle_cycle);
        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client =
            Client::tracked(rocket_with_state(battles.clone())).expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/set_lighting", id))
            .header(ContentType::Form)
            .body("set_lighting=operation_advance")
            .dispatch();
        assert_eq!(response.status(), Status::SeeOther);
        if let Phase::BattleCyclePhase(cycle) = &battles.lock().unwrap().get(&id).unwrap().phase {
            assert_eq!(cycle.lighting_condition, Some(Lighting::Night));
        };
    }

    #[test]
    fn reaction_player_can_choose_lighting_condition_at_first_cycle_given_intelligence_is_ambush() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Ambush;
        battle.phase =
            Phase::BattleCyclePhase(BattleCycle::new(Intelligence::Ambush, id.as_u128()));

        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .into_string()
            .unwrap();
        assert!(response_string.contains("Reaction player choose lighting"));
    }

    #[test]
    fn operation_player_can_choose_lighting_condition_at_first_cycle_given_intelligence_is_intercept(
    ) {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Intercept;
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .into_string()
            .unwrap();
        assert!(response_string.contains("Operation player choose lighting"));
    }

    #[test]
    fn operation_player_can_advance_lighting_by_2_slots_after_first_cycle_given_intelligence_is_surprise(
    ) {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Surprise;
        let mut battle_cycle = BattleCycle::new(Intelligence::Surprise, id.as_u128());
        battle_cycle.count = 3;
        battle.phase = Phase::BattleCyclePhase(battle_cycle);

        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .into_string()
            .unwrap();
        assert!(response_string.contains("Advance lighting 2 steps"));
    }

    #[test]
    fn reaction_player_cannot_choose_lighting_condition_after_first_cycle_even_when_intelligence_is_ambush(
    ) {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Ambush;
        let mut cycle = BattleCycle::intercept(id.as_u128());
        cycle.count = 2;
        battle.phase = Phase::BattleCyclePhase(cycle);

        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .into_string()
            .unwrap();
        assert!(response_string.contains("Advance lighting"));
    }

    #[test]
    fn determine_advantage() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));
        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/advantage_determination", id))
            .dispatch();
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[test]
    fn select_naval_movement_at_movement_segment() {
        let id = Uuid::new_v4();
        let mut battles_map = HashMap::new();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));
        battles_map.insert(id, battle);
        let battles = Arc::new(Mutex::new(battles_map));

        let client = Client::tracked(rocket_with_state(battles)).expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/movement", id))
            .header(ContentType::Form)
            .body("movement=NavalMovement")
            .dispatch();
        assert_eq!(response.status(), Status::SeeOther);
    }
}
