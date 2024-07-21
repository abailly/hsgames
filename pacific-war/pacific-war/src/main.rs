use chrono::NaiveDate;
use rocket::form;
use rocket::form::{Form, FromFormField, ValueField};
use rocket_dyn_templates::context;
use rocket_dyn_templates::Template;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::fmt::{Display, Formatter};

#[macro_use]
extern crate rocket;

#[get("/")]
fn index() -> Template {
    Template::render("index", context! {})
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, PartialEq, Serialize, Deserialize, FromFormField)]
enum Side {
    Japan,
    Allies,
}

impl Display for Side {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Side::Japan => write!(f, "japan"),
            Side::Allies => write!(f, "allies"),
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize, FromForm)]
struct NewBattle<'a> {
    battle_name: &'a str,
    start_date: NaiveDateForm,
    duration: u8,
    operation_player: Side,
}

impl<'a> NewBattle<'a> {
    fn to_form(&self) -> String {
        format!(
            "battle_name={}&start_date={}&duration={}&operation_player={}",
            self.battle_name, self.start_date.date, self.duration, self.operation_player
        )
    }
}

#[post("/battle", data = "<form>")]
fn battle(form: Form<NewBattle<'_>>) -> Template {
    let battle = form.into_inner();
    let date = battle.start_date.date.format("%Y-%m-%d").to_string();
    Template::render(
        "battle",
        context! {
            battle_name : battle.battle_name,
            start_date: date.clone(),
            current_date : date,
            duration: format!("{}", battle.duration),
            operation_player: battle.operation_player,
        },
    )
}

#[launch]
fn rocket() -> _ {
    rocket::build()
        .attach(Template::fairing())
        .mount("/", routes![index, battle])
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
    fn hello_world() {
        let client = Client::tracked(rocket()).expect("valid rocket instance");
        let battle = NewBattle {
            battle_name: "Coral Sea",
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
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        println!("{}", response_string);
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("Current date: 1942-05-01"));
        assert!(response_string.contains("21"));
    }
}
