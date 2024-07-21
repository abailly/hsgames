use rocket_dyn_templates::context;
use rocket_dyn_templates::Template;

#[macro_use]
extern crate rocket;

#[get("/")]
fn index() -> Template {
    Template::render("index", context! {})
}

#[get("/battle?<battle_name>&<start_date>&<duration>")]
fn battle(battle_name: &str, start_date: &str, duration: u8) -> Template {
    Template::render(
        "battle",
        context! {
        battle_name,
        start_date,
        duration,},
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
    use super::rocket;
    use rocket::http::Status;
    use rocket::local::blocking::Client;

    #[test]
    fn hello_world() {
        let client = Client::tracked(rocket()).expect("valid rocket instance");
        let mut response = client
            .get("/battle?battle_name=Coral+Sea&start_date=1942-05-01&duration=21")
            .dispatch();
        assert_eq!(response.status(), Status::Ok);
        let response_string = response.into_string().unwrap();
        assert!(response_string.contains("Coral Sea"));
        assert!(response_string.contains("1942-05-01"));
        assert!(response_string.contains("Current date: 1942-05-01"));
        assert!(response_string.contains("21"));
    }
}
