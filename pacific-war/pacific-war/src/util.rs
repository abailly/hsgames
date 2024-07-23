use chrono::NaiveDate;
use rocket::form;
use rocket::form::{FromFormField, ValueField};
use rocket::http::impl_from_uri_param_identity;
use rocket::http::uri::fmt::{Formatter, Path, UriDisplay};
use serde::{Deserialize, Serialize};
use std::fmt;
use uuid::Uuid;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, Copy)]
pub struct NaiveDateForm {
    pub date: NaiveDate,
}

#[rocket::async_trait]
impl<'v> FromFormField<'v> for NaiveDateForm {
    fn from_value(form_value: ValueField<'v>) -> form::Result<'v, Self> {
        Ok(NaiveDate::parse_from_str(&form_value.value, "%Y-%m-%d")
            .map(|date| NaiveDateForm { date })
            .map_err(|_| form::Error::validation("not a date"))?)
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct UuidForm {
    pub uuid: Uuid,
}

impl From<Uuid> for UuidForm {
    fn from(uuid: Uuid) -> Self {
        UuidForm { uuid }
    }
}

impl UriDisplay<Path> for UuidForm {
    fn fmt(&self, f: &mut Formatter<Path>) -> fmt::Result {
        f.write_raw(self.uuid.to_string())?;
        Ok(())
    }
}

impl_from_uri_param_identity!([Path] UuidForm);
