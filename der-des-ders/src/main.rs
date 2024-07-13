#![feature(assert_matches)]

use std::cmp::Ordering;
use std::io::{stdin, stdout};

mod tech;
use tech::*;

mod io;
use io::*;

mod side;
use side::*;

mod state;
use state::*;

mod fixtures;

enum PlayerType {
    Human,
    #[allow(dead_code)]
    Robot,
}

struct Options {
    allies: PlayerType,
    empires: PlayerType,
}

const DEFAULT_OPTIONS: Options = Options {
    allies: PlayerType::Human,
    empires: PlayerType::Human,
};

struct Players {
    allies_player: Box<dyn Player>,
    empires_player: Box<dyn Player>,
}

fn main() {
    let mut game_state = GameState::new(42);
    let mut players = initialise_players(DEFAULT_OPTIONS);
    while game_state.current_turn < 15 {
        run_turn(&mut players, &mut game_state);
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
        PlayerType::Robot => Box::new(RobotIO {}),
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

fn run_turn(players: &mut Players, game_state: &mut GameState) {
    players.output(&Output::CurrentState(game_state.to_owned()));
    determine_initiative(players, game_state);
    collect_resources(game_state);

    players.output(&Output::CurrentState(game_state.to_owned()));
    run_player_turn(game_state.initiative, players, game_state);
    run_player_turn(game_state.initiative.other(), players, game_state);

    let inp = players.allies_player.input();
    if let Input::Next = inp {
        game_state.current_turn += 1;
    }
}

fn run_player_turn(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    improve_technologies(initiative, players, game_state);
    launch_offensives(initiative, players, game_state);
    reinforcements(initiative, players, game_state);
    sea_control(initiative, players, game_state);
}

fn improve_technologies(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    let mut improved: Vec<TechnologyType> = vec![];

    while improved.len() < 4 {
        player.output(&Output::ImproveTechnologies);
        match player.input() {
            Input::Select(tech, n) => {
                if improved.contains(&tech) {
                    continue;
                }
                let die = game_state.roll();
                improve_technology(game_state, initiative, tech, n, die);
                improved.push(tech);
            }
            Input::Pass => break,
            other => player.output(&Output::WrongInput(other)),
        }
    }
}

fn improve_technology(
    game_state: &mut GameState,
    initiative: Side,
    tech: TechnologyType,
    n: u8,
    die: u8,
) {
    let year = game_state.current_year();
    let techs = &mut game_state
        .state_of_war
        .get_mut(&initiative)
        .unwrap()
        .technologies;
    let technologies = match initiative {
        Side::Allies => &ALLIES_TECHNOLOGIES,
        Side::Empires => &EMPIRE_TECHNOLOGIES,
    };
    match tech {
        TechnologyType::Attack => {
            let current = techs.attack;
            if let Some(technology) = &technologies[0][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.attack += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
        TechnologyType::Defense => {
            let current = techs.defense;
            if let Some(technology) = &technologies[1][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.defense += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
        TechnologyType::Artillery => {
            let current = techs.artillery;
            if let Some(technology) = &technologies[2][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.artillery += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
        TechnologyType::Air => {
            let current = techs.air;
            if let Some(technology) = &technologies[3][current as usize] {
                if year >= technology.date {
                    if die + n >= technology.min_dice_unlock {
                        techs.air += 1;
                    }
                    game_state.reduce_pr(initiative, n);
                }
            }
        }
    }
}

fn launch_offensives(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    let mut nations = game_state.all_nations_at_war(initiative);

    while !nations.is_empty() {
        player.output(&Output::LaunchOffensive);
        match player.input() {
            Input::Offensive(from, _, _) if !nations.contains(&from) => {
                player.output(&Output::CountryAlreadyAttacked(from));
            }
            Input::Offensive(from, to, _) if !from.adjacent_to(&to) => {
                player.output(&Output::AttackingNonAdjacentCountry(from, to));
            }
            Input::Offensive(from, to, pr) => {
                let operational = game_state.nations.get(&from).unwrap().operational_level();
                let resources = game_state.state_of_war.get(&initiative).unwrap().resources;
                if operational < pr {
                    player.output(&Output::OperationalLevelTooLow(operational, pr));
                } else if resources < pr {
                    player.output(&Output::NotEnoughResources(pr, resources));
                } else {
                    let offensive_result = resolve_offensive(game_state, initiative, from, to, pr);
                    game_state.reduce_pr(initiative, pr);
                    nations.retain(|&nat| nat != from);
                    player.output(&offensive_result);
                }
            }
            Input::Pass => return,
            _ => todo!(),
        }
    }
}

fn resolve_offensive(
    game_state: &mut GameState,
    initiative: Side,
    from: Nation,
    to: Nation,
    pr: u8,
) -> Output {
    let max_attacker_tech_level = game_state.countries.get(&from).unwrap().max_tech_level;
    let max_defender_tech_level = game_state.countries.get(&to).unwrap().max_tech_level;

    let artillery_bonus = game_state
        .artillery_bonus(&initiative)
        .min(max_attacker_tech_level);
    let attack_bonus = game_state
        .attack_bonus(&initiative)
        .min(max_attacker_tech_level);
    let defense_malus = game_state
        .defense_bonus(&initiative.other())
        .min(max_defender_tech_level);

    let dice: Vec<u8> = (0..pr).map(|_| game_state.roll()).collect();
    let artillery_dice: Vec<u8> = (0..artillery_bonus).map(|_| game_state.roll()).collect();

    let attack_country = |die: u8| {
        return die + attack_bonus - defense_malus
            >= game_state.countries.get(&from).unwrap().attack_factor;
    };

    let bomb_country =
        |die: u8| return die >= game_state.countries.get(&from).unwrap().attack_factor;

    let attack_hits = dice
        .iter()
        .map(|die| attack_country(*die))
        .filter(|hit| *hit)
        .count() as u8;
    let artillery_hits = artillery_dice
        .iter()
        .map(|die| bomb_country(*die))
        .filter(|hit| *hit)
        .count() as u8;

    let result = game_state.breakdown(&to, attack_hits + artillery_hits);

    Output::OffensiveResult { from, to, result }
}

fn sea_control(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    match initiative {
        Side::Empires => uboot(players, game_state),
        Side::Allies => blocus(players, game_state),
    };
}

fn uboot(players: &mut Players, game_state: &mut GameState) {
    let player = &mut players.empires_player;
    player.output(&Output::IncreaseUBoot);
    let bonus = match player.input() {
        Input::Number(n) => n.min(
            game_state
                .state_of_war
                .get(&Side::Empires)
                .unwrap()
                .resources,
        ),
        _ => 0,
    };

    let die = game_state.roll() + bonus;
    let loss = match die {
        1..=4 => 0,
        5 => 2,
        _ => 4,
    };

    let pr_lost = apply_hits(players, game_state, loss);

    game_state.reduce_pr(Side::Allies, pr_lost);
    game_state.reduce_pr(Side::Empires, bonus);
}

fn apply_hits(players: &mut Players, game_state: &mut GameState, loss: u8) -> u8 {
    players.output(&Output::UBootResult(loss));

    let allies_player = &mut players.allies_player;
    let pr = game_state
        .state_of_war
        .get(&Side::Allies)
        .unwrap()
        .resources;

    if loss > pr {
        let mut hits = loss - pr;
        while hits > 0 {
            allies_player.output(&Output::SelectNationForHit);
            if let Input::ApplyHit(nation) = allies_player.input() {
                game_state.breakdown(&nation, 1);
                hits -= 1;
            }
        }
        pr
    } else {
        loss
    }
}

fn blocus(players: &mut Players, game_state: &mut GameState) {
    let player = &mut players.allies_player;
    player.output(&Output::IncreaseBlockade);
    let bonus = match player.input() {
        Input::Number(n) => n.min(
            game_state
                .state_of_war
                .get(&Side::Allies)
                .unwrap()
                .resources,
        ),
        _ => 0,
    };

    let die = game_state.roll() + bonus;
    let gain = match die {
        1 => 3,
        2 => 1,
        _ => 0,
    };

    game_state.increase_pr(Side::Empires, gain);
    game_state.reduce_pr(Side::Allies, bonus);
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
fn determine_initiative(players: &mut Players, game_state: &mut GameState) {
    if game_state.current_turn > 1 {
        players.output(&Output::ChooseInitiative);
        let allies_pr = match players.allies_player.input() {
            Input::Number(pr) => pr,
            _ => 0,
        };
        let empires_pr = match players.empires_player.input() {
            Input::Number(pr) => pr,
            _ => 0,
        };
        let allies_initiative = allies_pr + game_state.roll();
        let empires_initiative = empires_pr + game_state.roll();

        game_state.initiative = match allies_initiative.cmp(&empires_initiative) {
            Ordering::Greater => Side::Allies,
            Ordering::Less => Side::Empires,
            Ordering::Equal => DEFAULT_INITIATIVE[game_state.current_turn as usize - 1],
        };

        game_state.reduce_pr(Side::Allies, allies_pr);
        game_state.reduce_pr(Side::Empires, empires_pr);
    }
    players.output(&Output::CurrentState(game_state.to_owned()));
}

fn collect_resources(game_state: &mut GameState) {
    game_state.increase_pr(Side::Allies, tally_resources(game_state, &Side::Allies));
    game_state.increase_pr(Side::Empires, tally_resources(game_state, &Side::Empires));
}

fn tally_resources(game_state: &GameState, pr_for_side: &Side) -> u8 {
    game_state
        .nations
        .iter()
        .fold(0, |acc, (nation, status)| match status {
            NationState::AtWar(breakdown) => match game_state.countries.get(nation) {
                Some(Country {
                    side, resources, ..
                }) if side == pr_for_side => {
                    acc + if *nation == Nation::Russia {
                        operational_level(*breakdown) * 2
                    } else {
                        *resources
                    }
                }
                _ => acc,
            },
            _ => acc,
        })
}

fn reinforcements(initiative: Side, players: &mut Players, game_state: &mut GameState) {
    let player = match initiative {
        Side::Allies => &mut players.allies_player,
        Side::Empires => &mut players.empires_player,
    };

    while game_state.state_of_war.get(&initiative).unwrap().resources > 0 {
        player.output(&Output::ReinforceNations);
        match player.input() {
            Input::Reinforce(nation, pr) => {
                game_state.reinforce(nation, pr);
            }
            Input::Pass => break,
            _ => continue,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        collect_resources, determine_initiative,
        fixtures::{PlayersBuilder, StateBuilder},
        GameState,
        Input::*,
        Nation::*,
        NationState::*,
        Side::*,
    };

    #[test]
    fn adjusts_resources_given_a_side_and_some_amount() {
        let mut state = GameState::new(12);
        state.increase_pr(Allies, 4);
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
        state.reduce_pr(Allies, 3);
        assert_eq!(1, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn cannot_reduce_resources_below_0() {
        let mut state = GameState::new(12);
        state.reduce_pr(Allies, 3);
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn empires_has_initiative_on_first_turn() {
        let mut state = StateBuilder::new(12).build();
        let mut players = PlayersBuilder::new().build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Empires, state.initiative)
    }

    #[test]
    fn allies_have_initiative_on_second_turn_given_they_bid_more_pr() {
        let mut state = StateBuilder::new(12).on_turn(2).build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(1))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Allies, state.initiative)
    }

    #[test]
    fn empires_have_initiative_on_second_turn_given_they_bid_more_pr() {
        let mut state = StateBuilder::new(12).on_turn(2).build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(1))
            .with_input(Empires, Number(2))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Empires, state.initiative)
    }

    #[test]
    fn initiative_consumes_bid_pr_from_both_sides() {
        let allies_bid = 1;
        let empires_bid = 2;
        let initial_allies_resources = 4;
        let initial_empires_resources = 5;

        let mut state = StateBuilder::new(12)
            .with_resources(Allies, initial_allies_resources)
            .with_resources(Empires, initial_empires_resources)
            .on_turn(2)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(allies_bid))
            .with_input(Empires, Number(empires_bid))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(
            initial_allies_resources - allies_bid,
            state.state_of_war.get(&Allies).unwrap().resources
        );
        assert_eq!(
            initial_empires_resources - empires_bid,
            state.state_of_war.get(&Empires).unwrap().resources
        )
    }

    #[test]
    fn allies_have_initiative_if_die_roll_is_better_given_equal_bid() {
        let mut state = StateBuilder::new(14)
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

        determine_initiative(&mut players, &mut state);

        assert_eq!(Allies, state.initiative)
    }

    #[test]
    fn empires_have_initiative_on_turn_2_given_equal_bid_and_die() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(2)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(4))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Empires, state.initiative)
    }

    #[test]
    fn allies_have_initiative_on_turn_4_given_equal_bid_and_die() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_resources(Empires, 5)
            .on_turn(4)
            .build();

        let mut players = PlayersBuilder::new()
            .with_input(Allies, Number(2))
            .with_input(Empires, Number(4))
            .build();

        determine_initiative(&mut players, &mut state);

        assert_eq!(Allies, state.initiative)
    }

    #[test]
    fn collect_resources_increase_pr_for_each_side() {
        let mut state = StateBuilder::new(14).build();

        collect_resources(&mut state);

        assert_eq!(14, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(9, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_cannot_increase_pr_over_20() {
        let mut state = StateBuilder::new(14).build();

        collect_resources(&mut state);
        collect_resources(&mut state);

        assert_eq!(20, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(18, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_changes_allies_pr_when_italy_goes_at_war() {
        let mut state = StateBuilder::new(14).with_nation(Italy, AtWar(5)).build();

        collect_resources(&mut state);

        assert_eq!(16, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(9, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_changes_allies_pr_when_russia_breakdown_changes() {
        let mut state = StateBuilder::new(14).with_nation(Russia, AtWar(3)).build();

        collect_resources(&mut state);

        assert_eq!(10, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(9, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn collect_resources_changes_empires_pr_when_bulgaria_goes_at_war() {
        let mut state = StateBuilder::new(14)
            .with_nation(Bulgaria, AtWar(3))
            .build();

        collect_resources(&mut state);

        assert_eq!(14, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(10, state.state_of_war.get(&Empires).unwrap().resources);
    }
}

#[cfg(test)]
mod technologies {

    use crate::{
        fixtures::{PlayersBuilder, StateBuilder},
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
        let mut state = StateBuilder::new(14).with_initiative(Empires).build();
        let mut players = PlayersBuilder::new().with_input(Empires, Pass).build();

        improve_technologies(Empires, &mut players, &mut state);

        assert_eq!(
            ZERO_TECHNOLOGIES,
            *state.state_of_war.get(&Empires).unwrap().technologies
        );
    }

    #[test]
    fn empires_improve_attack_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
            .with_resources(Empires, 4)
            .with_initiative(Empires)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Select(Attack, 2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Empires).unwrap().technologies
        );
        assert_eq!(2, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn empires_cannot_improve_attack_technology_too_soon() {
        let mut state = StateBuilder::new(14)
            .with_resources(Empires, 4)
            .with_initiative(Empires)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Select(Attack, 2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 0,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Empires).unwrap().technologies
        );
        assert_eq!(4, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn allies_improve_defense_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
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

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                defense: 2,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_cannot_improve_artillery_technology_1_level_before_year_available() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Artillery, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            ZERO_TECHNOLOGIES,
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_improve_attack_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
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

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 4,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_can_improve_technologies_1_level_until_pass() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Attack, 2))
            .with_input(Allies, Select(Artillery, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                attack: 1,
                artillery: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_cannot_improve_defense_technology_level_past_3() {
        let mut state = StateBuilder::new(14)
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

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                defense: 3,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
    }

    #[test]
    fn cannot_improve_same_technology_twice() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Select(Defense, 2))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                defense: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
    }

    #[test]
    fn allies_improve_air_technology_1_level_given_player_spends_resources() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(3)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Select(Air, 4))
            .with_input(Allies, Pass)
            .build();

        improve_technologies(Allies, &mut players, &mut state);

        assert_eq!(
            Technologies {
                air: 1,
                ..ZERO_TECHNOLOGIES
            },
            *state.state_of_war.get(&Allies).unwrap().technologies
        );
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn message_player_given_input_is_inappropriate_for_improve_tech_phase() {
        let mut state = StateBuilder::new(14).with_resources(Empires, 4).build();
        let mut players = PlayersBuilder::new()
            .with_input(Empires, Number(2))
            .with_input(Empires, Pass)
            .build();

        improve_technologies(Empires, &mut players, &mut state);

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
        fixtures::{PlayersBuilder, StateBuilder},
        launch_offensives, HitsResult,
        Input::*,
        Nation::*,
        NationState::*,
        Output,
        Side::*,
        Technologies, ZERO_TECHNOLOGIES,
    };

    #[test]
    fn initiative_player_can_spend_pr_to_launch_offensive_between_adjacent_countries() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
        assert_eq!(3, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn player_cannot_spend_more_pr_than_available_for_offensives() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 2)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 3))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::NotEnoughResources(3, 2),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(2, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn initiative_player_launch_several_offensives() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 1))
            .with_input(Allies, Offensive(Russia, OttomanEmpire, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
        assert_eq!(AtWar(4), *state.nations.get(&OttomanEmpire).unwrap());
        assert_eq!(2, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn cannot_launch_offensive_to_not_adjacent_country() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::AttackingNonAdjacentCountry(France, AustriaHungary),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn initiative_player_launch_cannot_launch_several_offensives_from_same_country() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Russia, Germany, 1))
            .with_input(Allies, Offensive(Russia, AustriaHungary, 1))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::OffensiveResult {
                    from: Russia,
                    to: Germany,
                    result: HitsResult::Hits(Germany, 1),
                },
                Output::LaunchOffensive,
                Output::CountryAlreadyAttacked(Russia),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(3, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn pr_for_offensive_cannot_exceed_operational_level() {
        let mut state = StateBuilder::new(16)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(Serbia, AustriaHungary, 2))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(
            vec![
                Output::LaunchOffensive,
                Output::OperationalLevelTooLow(1, 2),
                Output::LaunchOffensive,
            ],
            players.allies_player.out()
        );
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn attack_technology_provides_offensive_bonus() {
        let mut state = StateBuilder::new(18)
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

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn defense_technology_provides_offensive_malus() {
        let mut state = StateBuilder::new(18)
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

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(8), *state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn offensive_launch_as_many_dice_as_pr_expended() {
        let mut state = StateBuilder::new(15)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Offensive(France, Germany, 3))
            .with_input(Allies, Pass)
            .build();

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(5), *state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn artillery_technology_adds_more_dice_to_throw() {
        let mut state = StateBuilder::new(15)
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

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(5), *state.nations.get(&Germany).unwrap());
    }

    #[test]
    fn offensive_cannot_use_attack_technology_greater_than_limit() {
        let mut state = StateBuilder::new(11) // die roll < 3
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

        launch_offensives(Allies, &mut players, &mut state);

        assert_eq!(AtWar(5), *state.nations.get(&AustriaHungary).unwrap());
    }

    #[test]
    fn offensive_cannot_use_artillery_technology_greater_than_limit() {
        let mut state = StateBuilder::new(11) // die rolls = 2, 2, 4, 5
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

        launch_offensives(Allies, &mut players, &mut state);

        // max tech level of Serbia is 2
        // it should throw 4 dice but only 3 are taken into account
        // so no hit is inflicted
        assert_eq!(AtWar(5), *state.nations.get(&AustriaHungary).unwrap());
    }

    #[test]
    fn defensive_side_cannot_use_defense_technology_greater_than_limit() {
        let mut state = StateBuilder::new(14) // die = 6
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

        launch_offensives(Allies, &mut players, &mut state);

        // max tech level of OttomanEmpire is 2
        // attack factor of Russia is 5
        // result = 6 (die) + 1 (attack bonus) - 2 (defense bonus capped at max tech level) = 5
        // Russia inflicts 1 hit
        assert_eq!(AtWar(4), *state.nations.get(&OttomanEmpire).unwrap());
    }
}

#[cfg(test)]
mod reinforcements {

    use crate::{
        fixtures::{PlayersBuilder, StateBuilder},
        reinforcements,
        Input::*,
        Nation::*,
        NationState::*,
        Side::*,
    };

    #[test]
    fn initiative_player_can_spend_pr_to_reinforce_nation() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_nation(France, AtWar(4))
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Reinforce(France, 1))
            .with_input(Allies, Pass)
            .build();

        reinforcements(Allies, &mut players, &mut state);

        assert_eq!(AtWar(5), *state.nations.get(&France).unwrap());
        assert_eq!(3, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn reinforcements_cost_grows_quadratically() {
        let mut state = StateBuilder::new(14)
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

        reinforcements(Allies, &mut players, &mut state);

        assert_eq!(AtWar(5), *state.nations.get(&France).unwrap());
        assert_eq!(AtWar(5), *state.nations.get(&Russia).unwrap());
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn cannot_reinforce_nation_past_initial_breakdown() {
        let mut state = StateBuilder::new(14)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .with_nation(France, AtWar(6))
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new()
            .with_input(Allies, Reinforce(France, 3))
            .with_input(Allies, Pass)
            .build();

        reinforcements(Allies, &mut players, &mut state);

        assert_eq!(AtWar(7), *state.nations.get(&France).unwrap());
        assert_eq!(3, state.state_of_war.get(&Allies).unwrap().resources);
    }
}

#[cfg(test)]
mod sea {

    use crate::{
        fixtures::{PlayersBuilder, StateBuilder},
        sea_control,
        Input::*,
        Nation::*,
        NationState::*,
        Side::*,
    };

    #[test]
    fn empire_player_can_impact_resources_from_u_boot() {
        let mut state = StateBuilder::new(14) // die roll = 6
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Pass).build();

        sea_control(Empires, &mut players, &mut state);

        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_player_can_increase_resources_from_blocus() {
        let mut state = StateBuilder::new(11) // die roll = 2
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(2)
            .build();
        let mut players = PlayersBuilder::new().with_input(Allies, Pass).build();

        sea_control(Allies, &mut players, &mut state);

        assert_eq!(5, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn empires_player_can_spend_pr_to_increase_u_boot_die_roll() {
        let mut state = StateBuilder::new(11)
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Number(3)).build();

        sea_control(Empires, &mut players, &mut state);

        assert_eq!(2, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(1, state.state_of_war.get(&Empires).unwrap().resources);
    }

    #[test]
    fn modified_u_boot_die_roll_greater_than_6_is_6() {
        let mut state = StateBuilder::new(14)
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Number(3)).build();

        sea_control(Empires, &mut players, &mut state);

        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_player_can_spend_pr_to_increase_blocus_die_roll() {
        let mut state = StateBuilder::new(11)
            .with_resources(Empires, 4)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Allies, Number(1)).build();

        sea_control(Allies, &mut players, &mut state);

        assert_eq!(4, state.state_of_war.get(&Empires).unwrap().resources);
        assert_eq!(3, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_player_cannot_spend_more_pr_than_available_to_increase_blocus_die_roll() {
        let mut state = StateBuilder::new(11)
            .with_resources(Empires, 4)
            .with_resources(Allies, 1)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Allies, Number(2)).build();

        sea_control(Allies, &mut players, &mut state);

        assert_eq!(4, state.state_of_war.get(&Empires).unwrap().resources);
        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn empires_player_cannot_spend_more_pr_than_available_to_increase_u_boot_die_roll() {
        let mut state = StateBuilder::new(11)
            .with_resources(Empires, 2)
            .with_resources(Allies, 4)
            .with_initiative(Allies)
            .on_turn(1)
            .build();
        let mut players = PlayersBuilder::new().with_input(Empires, Number(3)).build();

        sea_control(Empires, &mut players, &mut state);

        assert_eq!(0, state.state_of_war.get(&Empires).unwrap().resources);
        assert_eq!(4, state.state_of_war.get(&Allies).unwrap().resources);
    }

    #[test]
    fn allies_player_need_to_increase_breakdown_given_they_don_t_have_enough_resources() {
        let mut state = StateBuilder::new(14)
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

        sea_control(Empires, &mut players, &mut state);

        assert_eq!(0, state.state_of_war.get(&Allies).unwrap().resources);
        assert_eq!(AtWar(6), *state.nations.get(&France).unwrap());
        assert_eq!(AtWar(5), *state.nations.get(&Russia).unwrap());
    }
}
