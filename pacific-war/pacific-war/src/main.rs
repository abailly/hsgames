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
use std::convert::From;
use std::sync::Arc;
use uuid::Uuid;

mod util;
use util::*;

mod core;
use core::*;

mod persistence;
use persistence::{BattleRepository, CachedRepository, PostgresRepository};

#[macro_use]
extern crate rocket;

#[get("/")]
fn index() -> Template {
    Template::render("index", context! {})
}

#[post("/battle", data = "<form>")]
async fn create_battle(
    service: &State<BattleService>,
    form: Form<NewBattle>
) -> Result<Redirect, Status> {
    let new_battle = form.into_inner();
    let uuid = Uuid::new_v4();
    let battle = Battle::new(uuid, &new_battle);

    service.repository.create(&battle)
        .await
        .map_err(|_| Status::InternalServerError)?;

    Ok(Redirect::to(uri!(battle(id = UuidForm::from(uuid)))))
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
async fn battle(service: &State<BattleService>, id: UuidForm) -> Result<Template, Status> {
    let battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    render_phase(&battle)
}

/// Update movement for contact phase
#[post("/battle/<id>/contact/<movement>")]
async fn contact_movement(
    service: &State<BattleService>,
    movement: MovementType,
    id: UuidForm,
) -> Result<Template, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    match &mut battle.phase {
        Phase::OperationContactPhase(_) | Phase::ReactionContactPhase(_) => {
            battle.contact_movement(&movement);

            service.repository.update(&battle)
                .await
                .map_err(|e| match e {
                    persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
                    _ => Status::InternalServerError,
                })?;

            render_phase(&battle)
        }
        _ => Err(Status::BadRequest),
    }
}

fn render_phase(battle: &Battle) -> Result<Template, Status> {
    match &battle.phase {
        Phase::StrategicPhase(phase_type) => Ok(render_strategic_phase(battle, phase_type)),
        Phase::OperationContactPhase(phase) => Ok(render_contact_phase(battle, phase)),
        Phase::ReactionActivationPhase(activation) => Ok(render_reaction_activation(battle, activation)),
        Phase::ReactionContactPhase(phase) => Ok(render_contact_phase(battle, phase)),
        Phase::BattleCyclePhase(phase) => Ok(render_battle_cycle(battle, phase)),
    }
}

fn render_strategic_phase(battle: &Battle, phase_type: &StrategicPhaseType) -> Template {
    let phase_index = match phase_type {
        StrategicPhaseType::Weather => 1,
        StrategicPhaseType::StrategicIntelligence => 2,
        StrategicPhaseType::StrategicBombing => 3,
        StrategicPhaseType::JapaneseEscort => 4,
        StrategicPhaseType::AlliedSubmarinePriority => 5,
        StrategicPhaseType::MerchantShippingAttrition => 6,
        StrategicPhaseType::CommandPoint => 7,
        StrategicPhaseType::IsolationPenalty => 8,
        StrategicPhaseType::StrategicTransport => 9,
        StrategicPhaseType::Reinforcement => 10,
        StrategicPhaseType::NavalRepair => 11,
        StrategicPhaseType::Replacement => 12,
        StrategicPhaseType::Engineering => 13,
        StrategicPhaseType::SubmarinePatrol => 14,
        StrategicPhaseType::OperationPlayerDetermination => 15,
        StrategicPhaseType::OperationPlayerActivation => 16,
        StrategicPhaseType::OperationalIntelligence => 17,
    };

    Template::render(
        "strategic_phase",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
            battle_name: &battle.battle_data.battle_name,
            start_date: &battle.battle_data.start_date.date,
            duration: &battle.battle_data.duration,
            current_date: &battle.current_date,
            phase_name: format!("{}", phase_type),
            phase_index: phase_index,
            parent: "battle",
        },
    )
}

fn render_reaction_activation(battle: &Battle, activation: &ReactionActivation) -> Template {
    Template::render(
        "reaction_activation",
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
            battle_name: &battle.battle_data.battle_name,
            start_date: &battle.battle_data.start_date.date,
            duration: &battle.battle_data.duration,
            current_date: &battle.current_date,
            phase_name: format!("{}", &battle.phase),
            activation: activation,
            parent: "battle",
        },
    )
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
        BattleCycleSegment::NavalCombats(phase) => {
            render_battle_cycle_naval_combat(battle, cycle, phase)
        }
        BattleCycleSegment::Bombardment => render_battle_cycle_phase(battle, cycle, "bombardment"),
        BattleCycleSegment::Demolition => render_battle_cycle_phase(battle, cycle, "demolition"),
        BattleCycleSegment::Rally => render_battle_cycle_phase(battle, cycle, "rally"),
        BattleCycleSegment::ActivationDeactivation => {
            render_battle_cycle_phase(battle, cycle, "deactivation")
        }
        _ => render_battle_cycle_default(battle, cycle),
    }
}

fn render_battle_cycle_naval_combat(
    battle: &Battle,
    cycle: &BattleCycle,
    naval_combat: &NavalCombat,
) -> Template {
    match &naval_combat.phase {
        NavalCombatPhase::NavalCombatDetermination => Template::render(
            "battle_cycle/naval_combat_determination",
            context! {
                battle_id: &battle.id,
                operation_player: &battle.battle_data.operation_player,
                intelligence_condition: &battle.battle_data.intelligence_condition,
                battle_name : &battle.battle_data.battle_name,
                start_date : &battle.battle_data.start_date.date,
                duration : &battle.battle_data.duration,
                current_date : &battle.current_date,
                cycle,
                cycle_phase: format!("{}", cycle.phase),
                naval_combat,
                parent: "battle",
            },
        ),
        _ => Template::render(
            "battle_cycle/naval_combat",
            context! {
                battle_id: &battle.id,
                operation_player: &battle.battle_data.operation_player,
                intelligence_condition: &battle.battle_data.intelligence_condition,
                battle_name : &battle.battle_data.battle_name,
                start_date : &battle.battle_data.start_date.date,
                duration : &battle.battle_data.duration,
                current_date : &battle.current_date,
                cycle,
                cycle_phase: format!("{}", cycle.phase),
                naval_combat,
                parent: "battle",
            },
        ),
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
            cycle,
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
            cycle,
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
            cycle,
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
            cycle,
            cycle_phase: format!("{}", cycle.phase),
        },
    )
}

fn render_battle_cycle_phase(battle: &Battle, cycle: &BattleCycle, phase: &str) -> Template {
    Template::render(
        format!("battle_cycle/{}", phase),
        context! {
            battle_id: &battle.id,
            operation_player: &battle.battle_data.operation_player,
            intelligence_condition: &battle.battle_data.intelligence_condition,
            battle_name : &battle.battle_data.battle_name,
            start_date : &battle.battle_data.start_date.date,
            duration : &battle.battle_data.duration,
            current_date : &battle.current_date,
            parent: "battle",
            cycle,
            cycle_phase: format!("{}", cycle.phase),
        },
    )
}

/// Update current phase
#[get("/battle/<id>/next")]
async fn battle_next(service: &State<BattleService>, id: UuidForm) -> Result<Template, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    battle.next();

    service.repository.update(&battle)
        .await
        .map_err(|e| match e {
            persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
            _ => Status::InternalServerError,
        })?;

    render_phase(&battle)
}

struct BattleService {
    repository: Arc<dyn persistence::BattleRepository>,
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
async fn set_lighting<'r>(
    service: &State<BattleService>,
    id: UuidForm,
    form: Form<SetLightingForm<'r>>,
) -> Result<Redirect, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

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

            service.repository.update(&battle)
                .await
                .map_err(|e| match e {
                    persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
                    _ => Status::InternalServerError,
                })?;

            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
}

#[post("/battle/<id>/advantage_determination")]
async fn advantage_determination(service: &State<BattleService>, id: UuidForm) -> Result<Redirect, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    match &mut battle.phase {
        Phase::BattleCyclePhase(_) => {
            battle.determine_advantage();
            battle.next();

            service.repository.update(&battle)
                .await
                .map_err(|e| match e {
                    persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
                    _ => Status::InternalServerError,
                })?;

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
async fn movement(
    service: &State<BattleService>,
    id: UuidForm,
    form: Form<MovementForm>,
) -> Result<Redirect, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    match &mut battle.phase {
        Phase::BattleCyclePhase(cycle) => {
            cycle.movement(&form.into_inner().movement);

            service.repository.update(&battle)
                .await
                .map_err(|e| match e {
                    persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
                    _ => Status::InternalServerError,
                })?;

            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
}

#[derive(FromForm)]
struct CombatDeterminationForm {
    hex_type: HexType,
    advantage: Option<Detection>,
    disadvantage: Option<Detection>,
}

impl FromFormField<'_> for HexType {
    fn from_value(field: ValueField<'_>) -> form::Result<'_, Self> {
        match field.value {
            "Coastal" => Ok(HexType::Coastal),
            "Open" => Ok(HexType::Open),
            "Restricted" => Ok(HexType::Restricted),
            _ => Err(
                form::Error::validation(format!("not a valid hex type {:?}", field.value)).into(),
            ),
        }
    }
}

impl FromFormField<'_> for Detection {
    fn from_value(field: ValueField<'_>) -> form::Result<'_, Self> {
        match field.value {
            "Detected" => Ok(Detection::Detected),
            "Undetected" => Ok(Detection::Undetected),
            _ => Err(
                form::Error::validation(format!("not a valid detection {:?}", field.value)).into(),
            ),
        }
    }
}

#[post("/battle/<id>/naval_combat_determination", data = "<form>")]
async fn naval_combat_determination(
    service: &State<BattleService>,
    id: UuidForm,
    form: Form<CombatDeterminationForm>,
) -> Result<Redirect, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    match &mut battle.phase {
        Phase::BattleCyclePhase(battle_cycle) => {
            let form = form.into_inner();
            battle_cycle.determine_naval_combat(
                &form.hex_type,
                &form.advantage.unwrap_or(Detection::Undetected),
                &form.disadvantage.unwrap_or(Detection::Undetected),
            );

            service.repository.update(&battle)
                .await
                .map_err(|e| match e {
                    persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
                    _ => Status::InternalServerError,
                })?;

            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
}

#[post("/battle/<id>/next_round")]
async fn next_naval_combat_round(service: &State<BattleService>, id: UuidForm) -> Result<Redirect, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    match &mut battle.phase {
        Phase::BattleCyclePhase(battle_cycle) => {
            battle_cycle.next_round();

            service.repository.update(&battle)
                .await
                .map_err(|e| match e {
                    persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
                    _ => Status::InternalServerError,
                })?;

            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
}

#[derive(FromForm)]
struct BidDistanceForm {
    advantage_bid: CombatDistance,
    disadvantage_bid: CombatDistance,
}

impl FromFormField<'_> for CombatDistance {
    fn from_value(field: ValueField<'_>) -> form::Result<'_, Self> {
        match field.value {
            "Short" => Ok(CombatDistance::Short),
            "Medium" => Ok(CombatDistance::Medium),
            "Long" => Ok(CombatDistance::Long),
            "Withdraw" => Ok(CombatDistance::Withdraw),
            _ => Err(form::Error::validation(format!(
                "not a valid combat distance {:?}",
                field.value
            ))
            .into()),
        }
    }
}

#[post("/battle/<id>/bid_distance", data = "<form>")]
async fn bid_distance(
    service: &State<BattleService>,
    id: UuidForm,
    form: Form<BidDistanceForm>,
) -> Result<Redirect, Status> {
    let mut battle = service.repository.get(id.uuid)
        .await
        .map_err(|_| Status::NotFound)?;

    match &mut battle.phase {
        Phase::BattleCyclePhase(battle_cycle) => {
            let form = form.into_inner();
            battle_cycle.bid_distance(&form.advantage_bid, &form.disadvantage_bid);

            service.repository.update(&battle)
                .await
                .map_err(|e| match e {
                    persistence::PersistenceError::ConcurrentModification(_) => Status::Conflict,
                    _ => Status::InternalServerError,
                })?;

            Ok(Redirect::to(uri!(battle(id))))
        }
        _ => Err(Status::BadRequest),
    }
}

#[launch]
async fn rocket() -> _ {
    let database_url = std::env::var("DATABASE_URL")
        .unwrap_or_else(|_| "postgres://localhost/pacific_war".to_string());

    let postgres_repo = PostgresRepository::new(&database_url)
        .await
        .expect("Failed to connect to database");

    postgres_repo.run_migrations()
        .await
        .expect("Failed to run migrations");

    let cached_repo = CachedRepository::new(postgres_repo);

    rocket_with_state(Arc::new(cached_repo))
}

fn rocket_with_state(
    repository: Arc<dyn persistence::BattleRepository>
) -> Rocket<Build> {
    rocket::build()
        .manage(BattleService { repository })
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
                movement,
                naval_combat_determination,
                next_naval_combat_round,
                bid_distance
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
    use rocket::local::asynchronous::Client;
    use persistence::InMemoryRepository;

    fn test_rocket() -> Rocket<Build> {
        let repo = InMemoryRepository::new();
        let cached = CachedRepository::new(repo);
        let repository: Arc<dyn persistence::BattleRepository> = Arc::new(cached);
        rocket_with_state(repository)
    }

    async fn test_rocket_with_battle(id: Uuid, battle: Battle) -> Rocket<Build> {
        let repo = InMemoryRepository::new();
        repo.create(&battle).await.unwrap();
        let cached = CachedRepository::new(repo);
        let repository: Arc<dyn persistence::BattleRepository> = Arc::new(cached);
        rocket_with_state(repository)
    }

    #[rocket::async_test]
    async fn can_serve_css_file() {
        let client = Client::tracked(test_rocket()).await.expect("valid rocket instance");
        let response = client.get("/public/pacific.css").dispatch().await;
        assert_eq!(response.status(), Status::Ok);
    }

    #[rocket::async_test]
    async fn post_battle_redirects_to_new_battle_with_uuid() {
        let client = Client::tracked(test_rocket()).await.expect("valid rocket instance");
        let battle = coral_sea();
        let response = client
            .post("/battle")
            .header(ContentType::Form)
            .body(battle.to_form())
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[rocket::async_test]
    async fn get_existing_battle_returns_template() {
        let id = Uuid::new_v4();
        let battle = coral_sea_battle(id);
        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client.get(format!("/battle/{}", id)).dispatch().await;
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().await.unwrap();
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

    #[rocket::async_test]
    async fn new_battle_starts_at_weather_phase() {
        let id = Uuid::new_v4();
        let battle = Battle::new(id, &coral_sea());
        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client.get(format!("/battle/{}", id)).dispatch().await;
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().await.unwrap();
        assert!(response_string.contains("Weather Phase"));
        assert!(response_string.contains("Strategic Phase 1 of 17"));
        assert!(response_string.contains("Next"));
    }

    #[rocket::async_test]
    async fn clicking_next_progresses_through_strategic_phases() {
        let id = Uuid::new_v4();
        let battle = Battle::new(id, &coral_sea());
        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");

        // Click next to move from Weather to Strategic Intelligence
        let response = client.get(format!("/battle/{}/next", id)).dispatch().await;
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().await.unwrap();
        assert!(response_string.contains("Strategic Intelligence Phase"));
        assert!(response_string.contains("Strategic Phase 2 of 17"));
    }

    #[rocket::async_test]
    async fn last_strategic_phase_transitions_to_operation_contact_phase() {
        let id = Uuid::new_v4();
        let mut battle = Battle::new(id, &coral_sea());
        // Set to last strategic phase
        battle.phase = Phase::StrategicPhase(StrategicPhaseType::OperationalIntelligence);

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");

        // Click next to move from OperationalIntelligence to OperationContactPhase
        let response = client.get(format!("/battle/{}/next", id)).dispatch().await;
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().await.unwrap();
        assert!(response_string.contains("Operation Contact Phase"));
        assert!(response_string.contains("GroundMovement"));
    }

    #[rocket::async_test]
    async fn choosing_ground_movement_at_contact_phase_removes_it_from_available_movements() {
        let id = Uuid::new_v4();
        let battle = coral_sea_battle(id);
        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/contact/GroundMovement", id))
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().await.unwrap();
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("21"));
        assert!(response_string.contains("Operation Contact Phase"));
        assert!(!response_string.contains(format!("{}/GroundMovement", id).as_str()));
        assert!(response_string.contains("AirMovement"));
        assert!(response_string.contains("NavalMovement"));
        assert!(response_string.contains("Next"));
    }

    #[rocket::async_test]
    async fn choosing_next_at_operation_contact_phase_moves_to_reaction_activation_phase() {
        let id = Uuid::new_v4();
        let battle = coral_sea_battle(id);
        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client.get(format!("/battle/{}/next", id)).dispatch().await;
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().await.unwrap();
        assert!(response_string.contains("Reaction Player Activation Phase"));
        assert!(response_string.contains("Next"));
    }

    #[rocket::async_test]
    async fn choosing_next_at_reaction_activation_phase_moves_to_reaction_contact_phase() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::ReactionActivationPhase(ReactionActivation {
            max_naval_movement_count: 3,
        });
        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client.get(format!("/battle/{}/next", id)).dispatch().await;
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().await.unwrap();
        assert!(response_string.contains("Reaction Contact Phase"));
        assert!(response_string.contains("GroundMovement"));
        assert!(response_string.contains("AirMovement"));
        assert!(response_string.contains("NavalMovement"));
        assert!(response_string.contains("Next"));
    }

    #[rocket::async_test]
    async fn can_choose_lighting_condition_when_battle_cycle_starts() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/set_lighting", id))
            .header(ContentType::Form)
            .body("lighting=Night&set_lighting=choose")
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[rocket::async_test]
    async fn advance_lighting_condition() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/set_lighting", id))
            .header(ContentType::Form)
            .body("set_lighting=advance")
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[rocket::async_test]
    async fn can_advance_lighting_condition_2_steps_only_once() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        let mut battle_cycle = BattleCycle::intercept(id.as_u128());
        battle_cycle.count = 3;
        battle_cycle.lighting_condition = Some(Lighting::DayPM);
        battle.phase = Phase::BattleCyclePhase(battle_cycle);

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/set_lighting", id))
            .header(ContentType::Form)
            .body("set_lighting=operation_advance")
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);

        // Get the updated battle from repository
        let service = client.rocket().state::<BattleService>().unwrap();
        let updated_battle = service.repository.get(id).await.unwrap();
        if let Phase::BattleCyclePhase(cycle) = &updated_battle.phase {
            assert_eq!(cycle.lighting_condition, Some(Lighting::Night));
        };
    }

    #[rocket::async_test]
    async fn reaction_player_can_choose_lighting_condition_at_first_cycle_given_intelligence_is_ambush() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Ambush;
        battle.phase =
            Phase::BattleCyclePhase(BattleCycle::new(Intelligence::Ambush, id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .await
            .into_string()
            .await
            .unwrap();
        assert!(response_string.contains("Reaction player choose lighting"));
    }

    #[rocket::async_test]
    async fn operation_player_can_choose_lighting_condition_at_first_cycle_given_intelligence_is_intercept(
    ) {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Intercept;
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .await
            .into_string()
            .await
            .unwrap();
        assert!(response_string.contains("Operation player choose lighting"));
    }

    #[rocket::async_test]
    async fn operation_player_can_advance_lighting_by_2_slots_after_first_cycle_given_intelligence_is_surprise(
    ) {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Surprise;
        let mut battle_cycle = BattleCycle::new(Intelligence::Surprise, id.as_u128());
        battle_cycle.count = 3;
        battle.phase = Phase::BattleCyclePhase(battle_cycle);

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .await
            .into_string()
            .await
            .unwrap();
        assert!(response_string.contains("Advance lighting 2 steps"));
    }

    #[rocket::async_test]
    async fn reaction_player_cannot_choose_lighting_condition_after_first_cycle_even_when_intelligence_is_ambush(
    ) {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.battle_data.intelligence_condition = Intelligence::Ambush;
        let mut cycle = BattleCycle::intercept(id.as_u128());
        cycle.count = 2;
        battle.phase = Phase::BattleCyclePhase(cycle);

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response_string = client
            .get(format!("/battle/{}", id))
            .dispatch()
            .await
            .into_string()
            .await
            .unwrap();
        assert!(response_string.contains("Advance lighting"));
    }

    #[rocket::async_test]
    async fn determine_advantage() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/advantage_determination", id))
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[rocket::async_test]
    async fn select_naval_movement_at_movement_segment() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/movement", id))
            .header(ContentType::Form)
            .body("movement=NavalMovement")
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[rocket::async_test]
    async fn determine_combat_status_at_start_of_naval_combat_phase() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/naval_combat_determination", id))
            .header(ContentType::Form)
            .body("advantage=Detected&disadvantage=Detected&hex_type=Coastal")
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[rocket::async_test]
    async fn move_to_next_combat_round() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client.post(format!("/battle/{}/next_round", id)).dispatch().await;
        assert_eq!(response.status(), Status::SeeOther);
    }

    #[rocket::async_test]
    async fn can_bid_combat_distance() {
        let id = Uuid::new_v4();
        let mut battle = coral_sea_battle(id);
        battle.phase = Phase::BattleCyclePhase(BattleCycle::intercept(id.as_u128()));

        let client = Client::tracked(test_rocket_with_battle(id, battle).await)
            .await
            .expect("valid rocket instance");
        let response = client
            .post(format!("/battle/{}/bid_distance", id))
            .header(ContentType::Form)
            .body("advantage_bid=Short&disadvantage_bid=Long")
            .dispatch()
            .await;
        assert_eq!(response.status(), Status::SeeOther);
    }
}
