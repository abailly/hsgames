#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Event {
    pub event_id: u8,
    pub year: u16,
    pub not_after: Option<u16>,
    pub title: &'static str,
}

pub const ALL_EVENTS: [Event; 13] = [
    Event {
        event_id: 1,
        year: 1914,
        not_after: None,
        title: "All is quiet",
    },
    Event {
        event_id: 2,
        year: 1914,
        not_after: None,
        title: "All is quiet",
    },
    Event {
        event_id: 3,
        year: 1914,
        not_after: Some(1914),
        title: "Schlieffen plan",
    },
    Event {
        event_id: 4,
        year: 1914,
        not_after: Some(1914),
        title: "Race to the sea",
    },
    Event {
        event_id: 5,
        year: 1915,
        not_after: None,
        title: "Shells crisis",
    },
    Event {
        event_id: 6,
        year: 1915,
        not_after: None,
        title: "Gas!",
    },
    Event {
        event_id: 7,
        year: 1915,
        not_after: None,
        title: "Von Lettow in Africa",
    },
    Event {
        event_id: 8,
        year: 1915,
        not_after: None,
        title: "Gallipoli",
    },
    Event {
        event_id: 9,
        year: 1915,
        not_after: None,
        title: "Towards separated peace?",
    },
    Event {
        event_id: 10,
        year: 1915,
        not_after: None,
        title: "Italy enters the war!",
    },
    Event {
        event_id: 11,
        year: 1915,
        not_after: None,
        title: "Bulgaria enters the war!",
    },
    Event {
        event_id: 12,
        year: 1915,
        not_after: None,
        title: "Lusitiania sank",
    },
    Event {
        event_id: 13,
        year: 1915,
        not_after: None,
        title: "All is quiet",
    },
];
