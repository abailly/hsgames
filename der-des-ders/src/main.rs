use clap::Parser;
use clap::ValueEnum;
use robot::RobotIO;
use std::cmp::Ordering;
use std::io::{stdin, stdout};
use std::process::exit;

mod tech;
use event::Event;
use tech::*;

mod io;
use io::*;

mod side;
use side::*;

mod state;
use state::*;

mod engine;
use engine::*;

mod event;
mod events;
mod fixtures;
mod logic;
mod robot;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum PlayerType {
    Human,
    Robot,
}

/// Sets types of player for allies and empires and optionally provide a seed
/// for dice rolling.
#[derive(Parser)]
struct Options {
    /// Player type for allies
    #[arg(long, value_enum, default_value_t = PlayerType::Human)]
    allies: PlayerType,
    /// Player type for empires
    #[arg(long, value_enum, default_value_t = PlayerType::Robot)]
    empires: PlayerType,
    /// Initial seed for dice rolling
    #[arg(short, long, default_value_t = 42)]
    seed: u64,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            allies: PlayerType::Human,
            empires: PlayerType::Human,
            seed: 42,
        }
    }
}

struct Players {
    allies_player: Box<dyn Player>,
    empires_player: Box<dyn Player>,
}

fn main() {
    let options = Options::parse();
    let mut game_engine = GameEngine::new(options.seed);
    let mut players = initialise_players(options);
    while !game_engine.game_ends() {
        run_turn(&mut players, &mut game_engine);
    }
    match game_engine.winner() {
        Side::Allies => exit(1),
        Side::Empires => exit(-1),
    }
}

fn initialise_players(default_options: Options) -> Players {
    let allies_player = make_player(Side::Allies, default_options.allies);
    let empires_player = make_player(Side::Empires, default_options.empires);
    Players {
        allies_player,
        empires_player,
    }
}

fn make_player(side: Side, player_type: PlayerType) -> Box<dyn Player> {
    match player_type {
        PlayerType::Human => Box::new(Console {
            side,
            inp: stdin(),
            outp: stdout(),
            out: vec![],
        }),
        PlayerType::Robot => Box::new(RobotIO::new(&side, 42)),
    }
}

impl Player for Players {
    fn output(&mut self, message: &Output) {
        self.allies_player.output(message);
        self.empires_player.output(message);
    }

    fn input(&mut self) -> Input {
        todo!()
    }

    fn out(&self) -> Vec<Output> {
        self.allies_player
            .out()
            .iter()
            .chain(self.empires_player.out().iter())
            .cloned()
            .collect()
    }
}

fn run_turn(players: &mut Players, game_engine: &mut GameEngine) {
    players.output(&Output::CurrentState(game_engine.state.clone()));
    determine_initiative(players, game_engine);
    draw_events(players, game_engine);
    game_engine.collect_resources();

    players.output(&Output::CurrentState(game_engine.state.clone()));

    run_player_turn(game_engine.state.initiative, players, game_engine);
    run_player_turn(game_engine.state.initiative.other(), players, game_engine);

    game_engine.new_turn();
}

fn draw_events(players: &mut Players, game_engine: &mut GameEngine) {
    let events = game_engine.draw_events();
    for event in events.iter() {
        players.output(&Output::EventDrawn(event.event_id, event.title.to_string()));
        apply_event(players, game_engine, event);
    }
}

fn apply_event(players: &mut Players, game_engine: &mut GameEngine, event: &Event) {
    match event.event_id {
        3 => {
            let offensive = Offensive {
                initiative: Side::Empires,
                from: Nation::Germany,
                to: Nation::France,
                pr: 2,
            };
            let result = game_engine.resolve_offensive(&offensive);
            players.output(&Output::OffensiveResult {
                from: Nation::Germany,
                to: Nation::France,
                result,
            });
        }
        _ => game_engine.play_events(event),
    }
}

fn run_player_turn(initiative: Side, players: &mut Players, game_engine: &mut GameEngine) {
    improve_technologies(initiative, players, game_engine);
    launch_offensives(initiative, players, game_engine);
    reinforcements(initiative, players, game_engine);
    sea_control(initiative, players, game_engine);
}

fn improve_technologies(initiative: Side, players: &mut Players, game_engine: &mut GameEngine) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    let mut improved: Vec<TechnologyType> = vec![];

    while improved.len() < 4 {
        player.output(&Output::ImproveTechnologies);
        match player.input() {
            Input::Select(tech, n) => {
                if improved.contains(&tech) || n == 0 {
                    continue;
                }
                let die = game_engine.roll();
                let output = improve_technology(game_engine, initiative, tech, n, die);
                player.output(&output);
                match output {
                    Output::ImprovedTechnology(tech, pr) => {
                        game_engine.improve_technology(&initiative, &tech);
                        game_engine.reduce_pr(initiative, pr);
                    }
                    Output::FailedTechnology(_, pr) => {
                        game_engine.reduce_pr(initiative, pr);
                    }
                    _ => {}
                }

                improved.push(tech);
            }
            Input::Pass => break,
            other => player.output(&Output::WrongInput(other)),
        }
    }
}

fn improve_technology(
    game_engine: &mut GameEngine,
    initiative: Side,
    tech: TechnologyType,
    n: u8,
    die: u8,
) -> Output {
    let year = game_engine.current_year();
    let techs = &mut game_engine
        .state
        .state_of_war
        .get_mut(&initiative)
        .unwrap()
        .technologies;
    let technologies = match initiative {
        Side::Allies => &ALLIES_TECHNOLOGIES,
        Side::Empires => &EMPIRE_TECHNOLOGIES,
    };

    improve_technology_result(technologies, techs.value(&tech), year, die, n, tech)
}

fn improve_technology_result(
    technologies_track: &[[Option<Technology>; 4]; 4],
    current_tech_level: u8,
    year: u16,
    die: u8,
    pr_spent: u8,
    tech_type: TechnologyType,
) -> Output {
    if current_tech_level == 4 {
        return Output::NoMoreTechnologyImprovement(tech_type, current_tech_level);
    }
    if let Some(technology) = &technologies_track[tech_type.index()][current_tech_level as usize] {
        if year >= technology.date {
            if die + pr_spent > technology.min_dice_unlock {
                Output::ImprovedTechnology(tech_type, pr_spent)
            } else {
                Output::FailedTechnology(tech_type, pr_spent)
            }
        } else {
            Output::TechnologyNotAvailable(technology.name.to_string(), technology.date, year)
        }
    } else {
        Output::NoMoreTechnologyImprovement(tech_type, current_tech_level)
    }
}

fn launch_offensives(initiative: Side, players: &mut Players, game_engine: &mut GameEngine) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    let mut nations = game_engine.all_nations_at_war(initiative);
    nations.sort();

    while !nations.is_empty() {
        player.output(&Output::LaunchOffensive(nations.clone()));
        match player.input() {
            Input::Offensive(from, _, _) if !nations.contains(&from) => {
                player.output(&Output::CountryAlreadyAttacked(from));
            }
            Input::Offensive(from, to, _) if !from.adjacent_to(&to) => {
                player.output(&Output::AttackingNonAdjacentCountry(from, to));
            }
            Input::Offensive(from, to, pr) => {
                let operational = game_engine
                    .state
                    .nations
                    .get(&from)
                    .unwrap()
                    .operational_level();
                let resources = game_engine
                    .state
                    .state_of_war
                    .get(&initiative)
                    .unwrap()
                    .resources;
                if operational < pr {
                    player.output(&Output::OperationalLevelTooLow(operational, pr));
                } else if resources < pr {
                    player.output(&Output::NotEnoughResources(pr, resources));
                } else {
                    let offensive = Offensive {
                        initiative,
                        from,
                        to,
                        pr,
                    };
                    let result = game_engine.resolve_offensive(&offensive);
                    nations.retain(|&nat| nat != from);
                    player.output(&Output::OffensiveResult { from, to, result });
                }
            }
            Input::Pass => return,
            _ => (),
        }
    }
}

fn sea_control(initiative: Side, players: &mut Players, game_engine: &mut GameEngine) {
    match initiative {
        Side::Empires => uboot(players, game_engine),
        Side::Allies => blocus(players, game_engine),
    };
}

fn uboot(players: &mut Players, game_engine: &mut GameEngine) {
    let player = &mut players.empires_player;
    player.output(&Output::IncreaseUBoot);
    let bonus = match player.input() {
        Input::Number(n) => n.min(game_engine.state.resources_for(&Side::Empires)),
        _ => 0,
    };

    let loss = game_engine.uboot_losses(bonus);

    let pr_lost = apply_hits(players, game_engine, loss);

    game_engine.reduce_pr(Side::Allies, pr_lost);
}

fn apply_hits(players: &mut Players, game_engine: &mut GameEngine, loss: u8) -> u8 {
    players.output(&Output::UBootResult(loss));

    let allies_player = &mut players.allies_player;
    let pr = game_engine.state.resources_for(&Side::Allies);

    if loss > pr {
        let mut hits = loss - pr;
        while hits > 0 {
            allies_player.output(&Output::SelectNationForHit);
            if let Input::ApplyHit(nation) = allies_player.input() {
                game_engine.apply_hits(&nation, 1);
                hits -= 1;
            }
        }
        pr
    } else {
        loss
    }
}

fn blocus(players: &mut Players, game_engine: &mut GameEngine) {
    let player = &mut players.allies_player;
    player.output(&Output::IncreaseBlockade);
    let bonus = match player.input() {
        Input::Number(n) => n.min(game_engine.state.resources_for(&Side::Allies)),
        _ => 0,
    };

    let gain = game_engine.blockade_effect(bonus);

    players.output(&Output::BlockadeResult(gain));
}

const DEFAULT_INITIATIVE: [Side; 14] = [
    Side::Empires,
    Side::Empires,
    Side::Empires,
    Side::Allies,
    Side::Empires,
    Side::Allies,
    Side::Allies,
    Side::Allies,
    Side::Allies,
    Side::Allies,
    Side::Empires,
    Side::Empires,
    Side::Allies,
    Side::Allies,
];

/// Decide whose player has the initiative
///
/// * On turn 1, the empires automatically have the initiative
/// * On subsequent turns, players bid PR for initiative and add a die roll. The player with the highest
///   total has the initiative. In case of a tie, the initiative is defined from the DEFAULT_INITIATIVE
///   array.
fn determine_initiative(players: &mut Players, game_engine: &mut GameEngine) {
    if game_engine.state.current_turn > 1 {
        players.output(&Output::ChooseInitiative);
        let allies_pr = match players.allies_player.input() {
            Input::Number(pr) => pr,
            _ => 0,
        };
        let empires_pr = match players.empires_player.input() {
            Input::Number(pr) => pr,
            _ => 0,
        };
        let allies_initiative = allies_pr + game_engine.roll();
        let empires_initiative = empires_pr + game_engine.roll();

        game_engine.state.initiative = match allies_initiative.cmp(&empires_initiative) {
            Ordering::Greater => Side::Allies,
            Ordering::Less => Side::Empires,
            Ordering::Equal => DEFAULT_INITIATIVE[game_engine.state.current_turn as usize - 1],
        };

        game_engine.reduce_pr(Side::Allies, allies_pr);
        game_engine.reduce_pr(Side::Empires, empires_pr);
    }
}

fn reinforcements(initiative: Side, players: &mut Players, game_engine: &mut GameEngine) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    while game_engine
        .state
        .state_of_war
        .get(&initiative)
        .unwrap()
        .resources
        > 0
    {
        player.output(&Output::ReinforceNations);
        match player.input() {
            Input::Reinforce(nation, pr) => {
                game_engine.reinforce(nation, pr);
            }
            Input::Pass => break,
            _ => continue,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        determine_initiative,
        fixtures::{EngineBuilder, PlayersBuilder},
        GameEngine,
        Input::*,
        Nation::*,
        NationState::*,
        Side::*,
    };

    #[test]
    fn adjusts_resources_given_a_side_and_some_amount() {
        let mut engine = GameEngine::new(12);
        engine.increase_pr(Allies, 4);
        assert_eq!(4, engine.state.state_of_war.get(&Allies).unwrap().resources);
        engine.reduce_pr(Allies, 3);
        assert_eq!(1, engine.state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn cannot_reduce_resources_below_0() {
        let mut engine = GameEngine::new(12);
        engine.reduce_pr(Allies, 3);
        assert_eq!(0, engine.state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn empires_has_initiative_on_first_turn() {
        let mut engine = EngineBuilder::new(12).build();
        let mut players = PlayersBuilder::new().build();

        determine_initiative(&mut players, &mut engine);

        assert_eq!(Empires, engine.state.initiative)
    }

    #[test]
    fn allies_have_initiative_on_second_turn_given_they_bid_more_pr() {
        let mut engine = EngineBuilder::new(12).on_turn(2).build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(1))
            .build();

        determine_initiative(&mut players, &mut engine);

        assert_eq!(Allies, engine.state.initiative)
    }

    #[test]
    fn empires_have_initiative_on_second_turn_given_they_bid_more_pr() {
        let mut engine = EngineBuilder::new(12).on_turn(2).build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(1))
            .with_input(Empires, Number(2))
            .build();

        determine_initiative(&mut players, &mut engine);

        assert_eq!(Empires, engine.state.initiative)
    }

    #[test]
    fn initiative_consumes_bid_pr_from_both_sides() {
        let allies_bid = 1;
        let empires_bid = 2;
        let initial_allies_resources = 4;
        let initial_empires_resources = 5;

        let mut engine = EngineBuilder::new(12)
            .with_resources(Allies, initial_allies_resources)
            .with_resources(Empires, initial_empires_resources)
            .on_turn(2)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(allies_bid))
            .with_input(Empires, Number(empires_bid))
            .build();

        determine_initiative(&mut players, &mut engine);

        assert_eq!(
            initial_allies_resources - allies_bid,
            engine.state.state_of_war.get(&Allies).unwrap().resources
        );
        assert_eq!(
            initial_empires_resources - empires_bid,
            engine.state.state_of_war.get(&Empires).unwrap().resources
        )
    }

    #[test]
    fn allies_have_initiative_if_die_roll_is_better_given_equal_bid() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(2)
            .build();

        let allies_bid = 2;
        let empires_bid = 2;

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(allies_bid))
            .with_input(Empires, Number(empires_bid))
            .build();

        determine_initiative(&mut players, &mut engine);

        assert_eq!(Allies, engine.state.initiative)
    }

    #[test]
    fn empires_have_initiative_on_turn_2_given_equal_bid_and_die() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(2)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(4))
            .build();

        determine_initiative(&mut players, &mut engine);

        assert_eq!(Empires, engine.state.initiative)
    }

    #[test]
    fn allies_have_initiative_on_turn_4_given_equal_bid_and_die() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(4)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(4))
            .build();

        determine_initiative(&mut players, &mut engine);

        assert_eq!(Allies, engine.state.initiative)
    }

    #[test]
    fn collect_resources_increase_pr_for_each_side() {
        let mut engine = EngineBuilder::new(14).build();

        engine.collect_resources();

        assert_eq!(14, engine.state.resources_for(&Allies));
        assert_eq!(9, engine.state.resources_for(&Empires));
    }

    #[test]
    fn collect_resources_cannot_increase_pr_over_20() {
        let mut engine = EngineBuilder::new(14).build();

        engine.collect_resources();
        engine.collect_resources();

        assert_eq!(20, engine.state.resources_for(&Allies));
        assert_eq!(18, engine.state.resources_for(&Empires));
    }

    #[test]
    fn collect_resources_changes_allies_pr_when_italy_goes_at_war() {
        let mut engine = EngineBuilder::new(14).with_nation(Italy, AtWar(5)).build();

        engine.collect_resources();

        assert_eq!(16, engine.state.resources_for(&Allies));
        assert_eq!(9, engine.state.resources_for(&Empires));
    }

    #[test]
    fn collect_resources_changes_allies_pr_when_russia_breakdown_changes() {
        let mut engine = EngineBuilder::new(14).with_nation(Russia, AtWar(3)).build();

        engine.collect_resources();

        assert_eq!(10, engine.state.resources_for(&Allies));
        assert_eq!(9, engine.state.resources_for(&Empires));
    }

    #[test]
    fn collect_resources_changes_empires_pr_when_bulgaria_goes_at_war() {
        let mut engine = EngineBuilder::new(14)
            .with_nation(Bulgaria, AtWar(3))
            .build();

        engine.collect_resources();

        assert_eq!(14, engine.state.resources_for(&Allies));
        assert_eq!(10, engine.state.resources_for(&Empires));
    }
}

#[cfg(test)]
mod events_tests {
    use crate::{
        apply_event, draw_events,
        event::ALL_EVENTS,
        fixtures::{EngineBuilder, PlayersBuilder},
        launch_offensives,
        Input::*,
        Nation::*,
        NationState::*,
        Output::*,
        Side::*,
    };

    #[test]
    fn draw_three_events_at_start_of_turn() {
        let mut engine = EngineBuilder::new(18).build();
        let mut players = PlayersBuilder::new().build();

        draw_events(&mut players, &mut engine);

        assert_eq!(
            vec![
                EventDrawn(2, "All is quiet".to_string()),
                EventDrawn(1, "All is quiet".to_string()),
                EventDrawn(3, "Schlieffen plan".to_string()),
            ],
            players
                .allies_player
                .out()
                .iter()
                .take(3)
                .cloned()
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn stop_drawing_events_if_no_events_are_available() {
        let mut engine = EngineBuilder::new(18).build();
        let mut players = PlayersBuilder::new().build();

        draw_events(&mut players, &mut engine);
        draw_events(&mut players, &mut engine);
        draw_events(&mut players, &mut engine);

        assert_eq!(5, players.allies_player.out().len());
    }

    #[test]
    fn add_events_from_year_to_pool_at_start_of_year_and_remove_invalid_events() {
        let mut engine = EngineBuilder::new(18).build();
        let mut players = PlayersBuilder::new().build();

        draw_events(&mut players, &mut engine);
        engine.new_turn(); // year = 1915
        draw_events(&mut players, &mut engine);

        // collect last 3 events drawn
        let events = players
            .allies_player
            .out()
            .iter()
            .skip(3)
            .filter_map(|e| match e {
                EventDrawn(eid, _) => ALL_EVENTS.iter().find(|ev| ev.event_id == *eid).cloned(),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(3, events.len());
        assert!(events
            .iter()
            .all(|ev| ev.not_after == Some(1915) || ev.not_after.is_none()));
    }

    #[test]
    fn applying_plan_schlieffen_immediately_runs_offensive_from_germany_to_france() {
        let mut engine = EngineBuilder::new(14).build();
        let mut players = PlayersBuilder::new().build();

        apply_event(&mut players, &mut engine, &ALL_EVENTS[2]);

        assert_eq!(AtWar(5), *engine.state.nations.get(&France).unwrap());
    }

    #[test]
    fn applying_race_to_the_sea_gives_a_bonus_to_offensive_between_france_and_germany() {
        let mut engine = EngineBuilder::new(20) // die roll = 3
            .with_initiative(Allies)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        apply_event(&mut players, &mut engine, &ALL_EVENTS[3]);
        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(7), *engine.state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn race_to_the_sea_is_deactivated_on_new_turn() {
        let mut engine = EngineBuilder::new(20) // die roll = 3
            .with_initiative(Allies)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        apply_event(&mut players, &mut engine, &ALL_EVENTS[3]);
        engine.new_turn();
        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(8), *engine.state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn italy_entering_war_updates_its_state() {
        let mut engine = EngineBuilder::new(14).build();
        let mut players = PlayersBuilder::new().build();

        apply_event(&mut players, &mut engine, &ALL_EVENTS[9]);

        assert_eq!(AtWar(5), *engine.state.nations.get(&Italy).unwrap());
    }
}

#[cfg(test)]
mod technologies {

    use crate::{
        fixtures::{EngineBuilder, PlayersBuilder},
        improve_technologies,
        Input::*,
        Output,
        Side::*,
        Technologies,
        TechnologyType::*,
        ZERO_TECHNOLOGIES,
    };

    #[test]
    fn technology_does_not_change_given_player_passes_on_it() {
        let mut engine = EngineBuilder::new(14).with_initiative(Empires).build();
        let mut players = PlayersBuilder::new().with_input(Empires, Pass).build();

        improve_technologies(Empires, &mut players, &mut engine);

        assert_eq!(
            ZERO_TECHNOLOGIES,
            *engine
                .state
                .state_of_war
                .get(&Empires)
                .unwrap()
                .technologies
        );
    }

    #[test]
    fn empires_improve_attack_technology_1_level_given_player_spends_resources() {
        let mut engine = EngineBuilder::new(11) // die roll = 2
            .with_resources(Empires, 4)
            .with_initiative(Empires)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Select(Attack, 3))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                attack: 1,
                ..ZERO_TECHNOLOGIES
            },
            *engine
                .state
                .state_of_war
                .get(&Empires)
                .unwrap()
                .technologies
        );
        assert_eq!(1, engine.state.resources_for(&Empires));
    }

    #[test]
    fn empires_cannot_improve_attack_technology_too_soon() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Empires, 4)
            .with_initiative(Empires)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Select(Attack, 2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                attack: 0,
                ..ZERO_TECHNOLOGIES
            },
            *engine
                .state
                .state_of_war
                .get(&Empires)
                .unwrap()
                .technologies
        );
        assert_eq!(4, engine.state.resources_for(&Empires));
        assert_eq!(
            vec![
                Output::ImproveTechnologies,
                Output::TechnologyNotAvailable("Combat Gas".to_string(), 1915, 1914),
                Output::ImproveTechnologies
            ],
            players.empires_player.out()
        );
    }

    #[test]
    fn allies_improve_defense_technology_1_level_given_player_spends_resources() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_technologies(
                Allies,
                Technologies {
                    defense: 1,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                defense: 2,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn cannot_improve_technology_without_spending_resources() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_technologies(
                Allies,
                Technologies {
                    defense: 1,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 0))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                defense: 1,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(4, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_cannot_improve_artillery_technology_1_level_before_year_available() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Artillery, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            ZERO_TECHNOLOGIES,
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(4, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_improve_attack_technology_1_level_given_player_spends_resources() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 3,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_initiative(Allies)
            .on_turn(11)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Attack, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                attack: 4,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_can_improve_technologies_1_level_until_pass() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Attack, 2))
            .with_input(Allies, Select(Artillery, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                attack: 1,
                artillery: 1,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_cannot_improve_defense_technology_level_past_3() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    defense: 3,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                defense: 3,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(
            vec![
                Output::ImproveTechnologies,
                Output::NoMoreTechnologyImprovement(Defense, 3),
                Output::ImproveTechnologies
            ],
            players.allies_player.out()
        );
    }

    #[test]
    fn allies_cannot_improve_attack_technology_level_past_4() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 4,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .on_turn(8)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Attack, 1))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                attack: 4,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(
            vec![
                Output::ImproveTechnologies,
                Output::NoMoreTechnologyImprovement(Attack, 4),
                Output::ImproveTechnologies
            ],
            players.allies_player.out()
        );
    }

    #[test]
    fn cannot_improve_same_technology_twice() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                defense: 1,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
    }

    #[test]
    fn allies_improve_air_technology_1_level_given_player_spends_resources() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(3)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Air, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut engine);

        assert_eq!(
            Technologies {
                air: 1,
                ..ZERO_TECHNOLOGIES
            },
            *engine.state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn message_player_given_input_is_inappropriate_for_improve_tech_phase() {
        let mut engine = EngineBuilder::new(14).with_resources(Empires, 4).build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Number(2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut engine);

        assert_eq!(
            vec![
                Output::ImproveTechnologies,
                Output::WrongInput(Number(2)),
                Output::ImproveTechnologies
            ],
            players.empires_player.out()
        );
    }
}

#[cfg(test)]
mod offensives {

    use crate::{
        fixtures::{EngineBuilder, PlayersBuilder},
        launch_offensives, HitsResult,
        Input::*,
        Nation::{self, *},
        NationState::*,
        Output,
        Side::*,
        Technologies, ZERO_TECHNOLOGIES,
    };

    const ALLIES_AT_START: [Nation; 5] = [France, Russia, Egypt, Serbia, FrenchAfrica];

    #[test]
    fn initiative_player_can_spend_pr_to_launch_offensive_between_adjacent_countries() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(7), *engine.state.nations.get(&Germany).unwrap());
        assert_eq!(3, engine.state.resources_for(&Allies));
    }

    #[test]
    fn player_cannot_spend_more_pr_than_available_for_offensives() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 2)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 3))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(
            vec![
                Output::LaunchOffensive(ALLIES_AT_START.to_vec()),
                Output::NotEnoughResources(3, 2),
                Output::LaunchOffensive(ALLIES_AT_START.to_vec()),
            ],
            players.allies_player.out()
        );
        assert_eq!(2, engine.state.resources_for(&Allies));
    }

    #[test]
    fn initiative_player_launch_several_offensives() {
        let mut engine = EngineBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Offensive(Russia, OttomanEmpire, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(7), *engine.state.nations.get(&Germany).unwrap());
        assert_eq!(AtWar(4), *engine.state.nations.get(&OttomanEmpire).unwrap());
        assert_eq!(2, engine.state.resources_for(&Allies));
    }

    #[test]
    fn prompt_list_possible_attacker() {
        let mut engine = EngineBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Offensive(Russia, OttomanEmpire, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(
            vec![
                Output::LaunchOffensive(vec![France, Russia, Egypt, Serbia, FrenchAfrica]),
                Output::OffensiveResult {
                    from: France,
                    to: Germany,
                    result: HitsResult::Hits(Germany, 1)
                },
                Output::LaunchOffensive(vec![Russia, Egypt, Serbia, FrenchAfrica]),
                Output::OffensiveResult {
                    from: Russia,
                    to: OttomanEmpire,
                    result: HitsResult::Hits(OttomanEmpire, 1)
                },
                Output::LaunchOffensive(vec![Egypt, Serbia, FrenchAfrica])
            ],
            players.allies_player.out()
        );
    }

    #[test]
    fn cannot_launch_offensive_to_not_adjacent_country() {
        let mut engine = EngineBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(
            vec![
                Output::LaunchOffensive(ALLIES_AT_START.to_vec()),
                Output::AttackingNonAdjacentCountry(France, AustriaHungary),
                Output::LaunchOffensive(ALLIES_AT_START.to_vec()),
            ],
            players.allies_player.out()
        );
        assert_eq!(4, engine.state.resources_for(&Allies));
    }

    #[test]
    fn initiative_player_launch_cannot_launch_several_offensives_from_same_country() {
        let mut engine = EngineBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Russia, Germany, 1))
            .with_input(Allies, Offensive(Russia, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(7), *engine.state.nations.get(&Germany).unwrap());
        assert_eq!(
            vec![
                Output::LaunchOffensive(ALLIES_AT_START.to_vec()),
                Output::OffensiveResult {
                    from: Russia,
                    to: Germany,
                    result: HitsResult::Hits(Germany, 1),
                },
                Output::LaunchOffensive([France, Egypt, Serbia, FrenchAfrica].to_vec()),
                Output::CountryAlreadyAttacked(Russia),
                Output::LaunchOffensive([France, Egypt, Serbia, FrenchAfrica].to_vec()),
            ],
            players.allies_player.out()
        );
        assert_eq!(3, engine.state.resources_for(&Allies));
    }

    #[test]
    fn pr_for_offensive_cannot_exceed_operational_level() {
        let mut engine = EngineBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Serbia, AustriaHungary, 2))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(
            vec![
                Output::LaunchOffensive(ALLIES_AT_START.to_vec()),
                Output::OperationalLevelTooLow(1, 2),
                Output::LaunchOffensive(ALLIES_AT_START.to_vec()),
            ],
            players.allies_player.out()
        );
        assert_eq!(4, engine.state.resources_for(&Allies));
    }

    #[test]
    fn attack_technology_provides_offensive_bonus() {
        let mut engine = EngineBuilder::new(18)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(7), *engine.state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn defense_technology_provides_offensive_malus() {
        let mut engine = EngineBuilder::new(18)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_technologies(
                Empires,
                Technologies {
                    defense: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(8), *engine.state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn offensive_launch_as_many_dice_as_pr_expended() {
        let mut engine = EngineBuilder::new(15)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 3))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(5), *engine.state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn artillery_technology_adds_more_dice_to_throw() {
        let mut engine = EngineBuilder::new(15)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    artillery: 2,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(5), *engine.state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn offensive_cannot_use_attack_technology_greater_than_limit() {
        let mut engine = EngineBuilder::new(11) // die roll < 3
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    attack: 3,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Serbia, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        assert_eq!(
            AtWar(5),
            *engine.state.nations.get(&AustriaHungary).unwrap()
        );
    }

    #[test]
    fn offensive_cannot_use_artillery_technology_greater_than_limit() {
        let mut engine = EngineBuilder::new(11) // die rolls = 2, 2, 4, 5
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Allies,
                Technologies {
                    artillery: 3,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Serbia, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        // max tech level of Serbia is 2
        // it should throw 4 dice but only 3 are taken into account
        // so no hit is inflicted
        assert_eq!(
            AtWar(5),
            *engine.state.nations.get(&AustriaHungary).unwrap()
        );
    }

    #[test]
    fn defensive_side_cannot_use_defense_technology_greater_than_limit() {
        let mut engine = EngineBuilder::new(14) // die = 6
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_technologies(
                Empires,
                Technologies {
                    defense: 3,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .with_technologies(
                Allies,
                Technologies {
                    attack: 1,
                    ..ZERO_TECHNOLOGIES
                },
            )
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Russia, OttomanEmpire, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut engine);

        // max tech level of OttomanEmpire is 2
        // attack factor of Russia is 5
        // result = 6 (die) + 1 (attack bonus) - 2 (defense bonus capped at max tech level) = 5
        // Russia inflicts 1 hit
        assert_eq!(AtWar(4), *engine.state.nations.get(&OttomanEmpire).unwrap());
    }
}

#[cfg(test)]
mod reinforcements {

    use crate::{
        fixtures::{EngineBuilder, PlayersBuilder},
        reinforcements,
        Input::*,
        Nation::*,
        NationState::*,
        Side::*,
    };

    #[test]
    fn initiative_player_can_spend_pr_to_reinforce_nation() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_nation(France, AtWar(4))
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Reinforce(France, 1))
            .with_input(Allies, Pass)
            .build();

        reinforcements(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(5), *engine.state.nations.get(&France).unwrap());
        assert_eq!(3, engine.state.resources_for(&Allies));
    }

    #[test]
    fn reinforcements_cost_grows_quadratically() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_nation(France, AtWar(4))
            .with_nation(Russia, AtWar(3))
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Reinforce(France, 2))
            .with_input(Allies, Reinforce(Russia, 3))
            .with_input(Allies, Pass)
            .build();

        reinforcements(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(5), *engine.state.nations.get(&France).unwrap());
        assert_eq!(AtWar(5), *engine.state.nations.get(&Russia).unwrap());
        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn cannot_reinforce_nation_past_initial_breakdown() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_nation(France, AtWar(6))
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Reinforce(France, 3))
            .with_input(Allies, Pass)
            .build();

        reinforcements(Allies, &mut players, &mut engine);

        assert_eq!(AtWar(7), *engine.state.nations.get(&France).unwrap());
        assert_eq!(3, engine.state.resources_for(&Allies));
    }
}

#[cfg(test)]
mod sea {

    use crate::{
        fixtures::{EngineBuilder, PlayersBuilder},
        sea_control,
        Input::*,
        Nation::*,
        NationState::*,
        Side::*,
    };

    #[test]
    fn empire_player_can_impact_resources_from_u_boot() {
        let mut engine = EngineBuilder::new(14) // die roll = 6
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Pass).build();

        sea_control(Empires, &mut players, &mut engine);

        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_player_can_increase_resources_from_blocus() {
        let mut engine = EngineBuilder::new(11) // die roll = 2
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new().with_input(Allies, Pass).build();

        sea_control(Allies, &mut players, &mut engine);

        assert_eq!(5, engine.state.resources_for(&Empires));
    }

    #[test]
    fn empires_player_can_spend_pr_to_increase_u_boot_die_roll() {
        let mut engine = EngineBuilder::new(11)
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Number(3)).build();

        sea_control(Empires, &mut players, &mut engine);

        assert_eq!(2, engine.state.resources_for(&Allies));
        assert_eq!(1, engine.state.resources_for(&Empires));
    }

    #[test]
    fn modified_u_boot_die_roll_greater_than_6_is_6() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Number(3)).build();

        sea_control(Empires, &mut players, &mut engine);

        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_player_can_spend_pr_to_increase_blocus_die_roll() {
        let mut engine = EngineBuilder::new(11)
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Allies, Number(1)).build();

        sea_control(Allies, &mut players, &mut engine);

        assert_eq!(4, engine.state.resources_for(&Empires));
        assert_eq!(3, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_player_cannot_spend_more_pr_than_available_to_increase_blocus_die_roll() {
        let mut engine = EngineBuilder::new(11)
            .with_resources(Empires, 4)
            .with_resources(Allies, 1)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Allies, Number(2)).build();

        sea_control(Allies, &mut players, &mut engine);

        assert_eq!(4, engine.state.resources_for(&Empires));
        assert_eq!(0, engine.state.resources_for(&Allies));
    }

    #[test]
    fn empires_player_cannot_spend_more_pr_than_available_to_increase_u_boot_die_roll() {
        let mut engine = EngineBuilder::new(11)
            .with_resources(Empires, 2)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Number(3)).build();

        sea_control(Empires, &mut players, &mut engine);

        assert_eq!(0, engine.state.resources_for(&Empires));
        assert_eq!(4, engine.state.resources_for(&Allies));
    }

    #[test]
    fn allies_player_need_to_increase_breakdown_given_they_don_t_have_enough_resources() {
        let mut engine = EngineBuilder::new(14)
            .with_resources(Empires, 4)
            .with_resources(Allies, 1)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Pass)
            .with_input(Allies, ApplyHit(France))
            .with_input(Allies, ApplyHit(Russia))
            .with_input(Allies, ApplyHit(Russia))
            .build();

        sea_control(Empires, &mut players, &mut engine);

        assert_eq!(0, engine.state.resources_for(&Allies));
        assert_eq!(AtWar(6), *engine.state.nations.get(&France).unwrap());
        assert_eq!(AtWar(5), *engine.state.nations.get(&Russia).unwrap());
    }
}
