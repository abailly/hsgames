use crate::event::Event;
use crate::events::*;
use crate::logic::*;
use crate::side::*;
use crate::state::*;
use std::mem::swap;

pub struct GameEngine {
    pub(crate) state: GameState,
    logic: Box<dyn GameLogic>,
    played_events: Vec<ActiveEvent>,
}

impl GameEngine {
    pub fn new(seed: u64) -> Self {
        GameEngine {
            state: GameState::new(seed),
            logic: Box::new(default_game_logic()),
            played_events: Vec::new(),
        }
    }

    pub fn with_state(state: GameState) -> GameEngine {
        GameEngine {
            state,
            logic: Box::new(default_game_logic()),
            played_events: Vec::new(),
        }
    }

    pub fn collect_resources(&mut self) {
        self.logic.collect_resources(&mut self.state);
    }

    pub fn reduce_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        self.logic.reduce_pr(&mut self.state, &side, pr);
        self
    }

    pub fn increase_pr(&mut self, side: Side, pr: u8) -> &mut Self {
        self.state.increase_pr(side, pr);
        self
    }

    pub fn roll(&mut self) -> u8 {
        self.state.roll()
    }

    pub fn current_year(&self) -> u16 {
        self.state.current_year()
    }

    pub fn all_nations_at_war(&self, initiative: Side) -> Vec<Nation> {
        self.state.all_nations_at_war(initiative)
    }

    pub(crate) fn reinforce(&mut self, nation: Nation, pr: u8) -> &Self {
        let nation_state = self.state.nations.get_mut(&nation).unwrap();
        let maximum_breakdown = nation.maximum_breakdown();
        let current_breakdown = nation_state.breakdown();

        let (spent, reinforcement) =
            (1..=(pr + 1)).fold((0, 0), |(spent, reinforcement), resource| {
                if spent + resource <= pr && reinforcement + current_breakdown < maximum_breakdown {
                    (spent + resource, reinforcement + 1)
                } else {
                    (spent, reinforcement)
                }
            });
        nation_state.reinforce(reinforcement);
        self.reduce_pr(nation.side(), spent);
        self
    }

    pub(crate) fn apply_hits(&mut self, to: &Nation, hits: u8) -> HitsResult {
        self.logic.apply_hits(&mut self.state, to, hits)
    }

    pub(crate) fn draw_events(&mut self) -> Vec<Event> {
        self.state.draw_events()
    }

    pub(crate) fn resolve_offensive(&mut self, offensive: &Offensive) -> HitsResult {
        let (artillery_bonus, attack_bonus, defense_malus) = self.compute_bonus(offensive);

        let dice: Vec<u8> = self.roll_offensive_dice(offensive.pr);
        let artillery_dice: Vec<u8> = self.roll_artillery_dice(artillery_bonus);

        let attack_hits = self.evaluate_attack_hits(attack_bonus, defense_malus, offensive, &dice);

        let artillery_hits = self.evaluate_artillery_hits(offensive, &artillery_dice);

        self.reduce_pr(offensive.initiative, offensive.pr);
        self.apply_hits(&offensive.to, attack_hits + artillery_hits)
    }

    fn evaluate_artillery_hits(&mut self, offensive: &Offensive, artillery_dice: &Vec<u8>) -> u8 {
        self.logic
            .evaluate_artillery_hits(&self.state, offensive, artillery_dice)
    }

    pub fn evaluate_attack_hits(
        &mut self,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice: &Vec<u8>,
    ) -> u8 {
        self.logic.evaluate_attack_hits(
            &mut self.state,
            attack_bonus,
            defense_malus,
            offensive,
            dice,
        )
    }

    pub fn roll_artillery_dice(&mut self, artillery_bonus: u8) -> Vec<u8> {
        self.logic
            .roll_artillery_dice(&mut self.state, artillery_bonus)
    }

    pub fn roll_offensive_dice(&mut self, pr: u8) -> Vec<u8> {
        self.logic.roll_offensive_dice(&mut self.state, pr)
    }

    pub fn compute_bonus(&mut self, offensive: &Offensive) -> (u8, i8, i8) {
        self.logic.compute_bonus(&self.state, offensive)
    }

    pub fn uboot_losses(&mut self, bonus: u8) -> u8 {
        self.logic.uboot_losses(&mut self.state, bonus)
    }

    pub(crate) fn new_turn(&mut self) -> &mut Self {
        self.logic.new_turn(&mut self.state);
        self
    }

    pub(crate) fn play_events(&mut self, event: &Event) {
        let active_event = self.play(event);
        self.played_events.push(active_event);
    }

    fn play(&mut self, event: &Event) -> ActiveEvent {
        match event.event_id {
            4 => self.make_event_active(RaceToTheSea::new),
            5 => self.make_event_active(ShellCrisis::new),
            6 => self.make_event_active(Gas::new),
            7 => self.make_event_active(VonLettowInAfrica::new),
            8 => self.make_event_active(Gallipoli::new),
            9 => self.make_event_active(SeparatePeace::new),
            10 => {
                self.state
                    .nations
                    .insert(Nation::Italy, NationState::AtWar(5));
            }
            11 => {
                self.state
                    .nations
                    .insert(Nation::Bulgaria, NationState::AtWar(3));
            }
            14 => match self.roll() {
                1 => {
                    self.increase_pr(Side::Empires, 3);
                }
                5 => {
                    self.make_event_active(GermanFleetDefeated::new);
                }
                6 => {
                    self.make_event_active(GermanFleetDestroyed::new);
                }
                _ => {}
            },
            15 => self.make_event_active(TrentinOffensive::new),
            16 => self.make_event_active(WoodrowWilson::new),
            19 => self.make_event_active(BrusilovOffensive::new),
            20 => {
                self.state
                    .nations
                    .insert(Nation::Romania, NationState::AtWar(3));
            }
            22 => self.make_event_active(Mutinies::new),
            23 => self.make_event_active(GazaOffensive::new),
            24 => {
                self.reduce_pr(Side::Empires, 2);
            }
            25 => self.make_event_active(BattleOfCaporetto::new),
            _ => {}
        }
        let active_event = ActiveEvent {
            event: event.clone(),
            deactivation: |_game| true, // by default, events last for one turn
        };
        self.logic.event_activated(&active_event);
        active_event
    }

    fn make_event_active<T>(&mut self, new: fn(Box<dyn GameLogic>) -> T)
    where
        T: GameLogic + 'static,
    {
        let mut previous: Box<dyn GameLogic> = Box::new(DummyLogic::new());
        swap(&mut previous, &mut self.logic);
        self.logic = Box::new((new)(previous));
    }

    pub(crate) fn blockade_effect(&mut self, bonus: u8) -> u8 {
        self.logic.blockade_effect(&mut self.state, bonus)
    }
}

struct DefaultGameLogic {}

impl GameLogic for DefaultGameLogic {
    fn collect_resources(&mut self, state: &mut GameState) {
        state.increase_pr(Side::Allies, state.tally_resources(&Side::Allies));
        state.increase_pr(Side::Empires, state.tally_resources(&Side::Empires));
    }

    fn compute_bonus(&mut self, state: &GameState, offensive: &Offensive) -> (u8, i8, i8) {
        let max_attacker_tech_level = state.countries.get(&offensive.from).unwrap().max_tech_level;
        let max_defender_tech_level = state.countries.get(&offensive.to).unwrap().max_tech_level;

        let artillery_bonus = state
            .artillery_bonus(&offensive.initiative)
            .min(max_attacker_tech_level);
        let attack_bonus = state
            .attack_bonus(&offensive.initiative)
            .min(max_attacker_tech_level) as i8;

        let defense_malus = state
            .defense_bonus(&offensive.initiative.other())
            .min(max_defender_tech_level) as i8;
        (artillery_bonus, attack_bonus, defense_malus)
    }

    fn roll_offensive_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        (0..num).map(|_| state.roll()).collect()
    }

    fn roll_artillery_dice(&mut self, state: &mut GameState, num: u8) -> Vec<u8> {
        (0..num).map(|_| state.roll()).collect()
    }
    fn evaluate_attack_hits(
        &mut self,
        state: &mut GameState,
        attack_bonus: i8,
        defense_malus: i8,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        {
            let attack_country = |die: u8| {
                return die as i8 + attack_bonus - defense_malus
                    >= state.countries.get(&offensive.from).unwrap().attack_factor as i8;
            };
            let attack_hits = dice_roll
                .iter()
                .map(|die| attack_country(*die))
                .filter(|hit| *hit)
                .count() as u8;
            attack_hits
        }
    }

    fn evaluate_artillery_hits(
        &mut self,
        state: &GameState,
        offensive: &Offensive,
        dice_roll: &Vec<u8>,
    ) -> u8 {
        {
            let bomb_country =
                |die: u8| return die >= state.countries.get(&offensive.from).unwrap().attack_factor;
            let artillery_hits = dice_roll
                .iter()
                .map(|die| bomb_country(*die))
                .filter(|hit| *hit)
                .count() as u8;
            artillery_hits
        }
    }

    fn reduce_pr(&mut self, state: &mut GameState, side: &Side, pr: u8) {
        {
            let st = state.state_of_war.get_mut(side).unwrap();
            if st.resources >= pr {
                st.resources -= pr;
            }
        };
    }

    fn apply_hits(&mut self, state: &mut GameState, nation: &Nation, hits: u8) -> HitsResult {
        if let NationState::AtWar(breakdown) = state.nations.get_mut(nation).unwrap() {
            if hits >= *breakdown {
                state.surrenders(nation)
            } else {
                *breakdown -= hits;
                HitsResult::Hits(*nation, hits)
            }
        } else {
            HitsResult::NationNotAtWar(*nation)
        }
    }

    fn new_turn(&mut self, state: &mut GameState) {
        let current_turn_year = state.current_year();
        state.current_turn += 1;
        let next_year = state.current_year();
        if next_year != current_turn_year {
            state.new_year(current_turn_year, next_year);
        }
    }

    fn uboot_losses(&mut self, state: &mut GameState, bonus: u8) -> u8 {
        let die = state.roll() + bonus;
        let loss = match die {
            1..=4 => 0,
            5 => 2,
            _ => 4,
        };
        self.reduce_pr(state, &Side::Empires, bonus);
        loss
    }

    fn blockade_effect(&mut self, state: &mut GameState, bonus: u8) -> u8 {
        let die = state.roll() + bonus;
        let gain = match die {
            1 => 3,
            2 => 1,
            _ => 0,
        };

        state.increase_pr(Side::Empires, gain);
        self.reduce_pr(state, &Side::Allies, bonus);

        gain
    }
    fn event_activated(&mut self, _event: &ActiveEvent) {}
}

fn default_game_logic() -> impl GameLogic {
    DefaultGameLogic {}
}

#[cfg(test)]
mod engine_test {
    use crate::{event::ALL_EVENTS, fixtures::EngineBuilder};

    #[test]
    fn played_events_stay_between_turns() {
        let mut engine = EngineBuilder::new(11).build();

        // activate "Race to the sea"
        engine.play_events(&ALL_EVENTS[3]);
        engine.new_turn();

        assert_eq!(1, engine.played_events.len());
    }
}
