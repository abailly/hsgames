use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::io::{Input, Output, Player};
use crate::{GameState, Nation, Side, TechnologyType};

pub struct RobotIO {
    pub state: Option<GameState>,
    pub side: Side,
    pub phase: Option<Output>,
    rng: StdRng,
}

impl RobotIO {
    pub fn new(side: &Side, seed: u64) -> Self {
        RobotIO {
            state: None,
            side: side.clone(),
            phase: None,
            rng: StdRng::seed_from_u64(seed),
        }
    }

    fn possible_offensives(&self, sources: &[Nation]) -> Vec<Input> {
        let state = self.state.as_ref().unwrap();
        let resources = state.state_of_war.get(&self.side).unwrap().resources;
        let mut possible_plays: Vec<Input> = Vec::new();

        for source in sources.iter() {
            let mut targets: Vec<&Nation> = state.neighbours(source);
            targets.sort();
            for target in targets.iter() {
                for pr in 1..=state.operational_level(source) {
                    if pr <= resources {
                        possible_plays.push(Input::Offensive(*source, **target, pr))
                    };
                }
            }
        }
        possible_plays.push(Input::Pass);
        possible_plays
    }

    fn possible_tech_improvements(&self, techs: &[TechnologyType]) -> Vec<Input> {
        let state = self.state.as_ref().unwrap();
        let resources = state.state_of_war.get(&self.side).unwrap().resources;
        let max_pr = resources.min(4);
        let mut possible_plays = state
            .available_technologies(&self.side)
            .iter()
            .filter(|t| techs.contains(&t.category))
            .flat_map(|t| (1..=max_pr).map(move |pr| Input::Select(t.category, pr)))
            .collect::<Vec<Input>>();
        possible_plays.push(Input::Pass);
        possible_plays
    }
}

impl Player for RobotIO {
    fn output(&mut self, message: &Output) {
        match message {
            Output::CurrentState(state) => {
                self.state = Some(state.clone());
            }
            Output::ChooseInitiative => {
                self.phase = Some(message.clone());
            }
            Output::ImproveTechnologies(_) => {
                self.phase = Some(message.clone());
            }
            Output::LaunchOffensive(_) => {
                self.phase = Some(message.clone());
            }
            Output::ReinforceNations => {
                self.phase = Some(message.clone());
            }
            Output::WrongInput(_) => {}
            Output::NotEnoughResources(_, _) => {}
            Output::CountryAlreadyAttacked(_) => {}
            Output::AttackingNonAdjacentCountry(_, _) => {}
            Output::OperationalLevelTooLow(_, _) => {}
            Output::OffensiveResult {
                from: _,
                to: _,
                result: _,
            } => {}
            Output::IncreaseUBoot => {
                self.phase = Some(message.clone());
            }
            Output::UBootResult(_) => {}
            Output::IncreaseBlockade => {
                self.phase = Some(message.clone());
            }
            Output::BlockadeResult(_) => {}
            Output::SelectNationForHit => {
                self.phase = Some(message.clone());
            }
            Output::EventDrawn(_, _) => {}
            Output::ImprovedTechnology(_, _) => {}
            Output::FailedTechnology(_, _) => {}
            Output::TechnologyNotAvailable(_, _, _) => {}
            Output::NoMoreTechnologyImprovement(_, _) => {}
            Output::TurnFor(_, _) => {}
        }
    }

    fn input(&mut self) -> Input {
        match &self.phase {
            Some(Output::ChooseInitiative) => {
                let resources = self
                    .state
                    .as_ref()
                    .unwrap()
                    .state_of_war
                    .get(&self.side)
                    .unwrap()
                    .resources;
                let max_pr = resources.min(3);
                let pr = self.rng.gen_range(0..=max_pr);
                Input::Number(pr)
            }
            Some(Output::ImproveTechnologies(techs)) => {
                let possible_plays = self.possible_tech_improvements(techs);
                possible_plays[self.rng.gen_range(0..possible_plays.len())]
            }
            Some(Output::LaunchOffensive(sources)) => {
                let possible_plays = self.possible_offensives(sources);
                if possible_plays.is_empty() {
                    Input::Pass
                } else {
                    possible_plays[self.rng.gen_range(0..possible_plays.len())]
                }
            }
            Some(Output::ReinforceNations) => {
                let state = self.state.as_ref().unwrap();
                let resources = state.state_of_war.get(&self.side).unwrap().resources;
                let nations = state.all_nations_at_war(self.side);
                let mut nations = nations
                    .iter()
                    .filter_map(|n| {
                        let losses = n.maximum_breakdown() - state.breakdown_level(n);
                        if losses > 0 {
                            Some((*n, losses))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<(Nation, u8)>>();
                if nations.is_empty() {
                    return Input::Pass;
                }
                nations.sort();
                let nation_index = self.rng.gen_range(0..nations.len());
                let (nation, loss) = nations[nation_index];
                let pr = self.rng.gen_range(0..=resources.min(loss));
                if pr > 0 {
                    Input::Reinforce(nation, pr)
                } else {
                    Input::Pass
                }
            }
            Some(Output::IncreaseUBoot) => Input::Number(0),
            Some(Output::IncreaseBlockade) => Input::Number(0),
            Some(Output::SelectNationForHit) => {
                let state = self.state.as_ref().unwrap();
                let nations = state.all_nations_at_war(self.side);
                let mut nations = nations
                    .iter()
                    .filter_map(|n| {
                        if state.breakdown_level(n) > 0 {
                            Some((*n, state.breakdown_level(n)))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<(Nation, u8)>>();
                nations.sort_by(|a, b| a.1.cmp(&b.1).reverse());
                let nation_index = self.rng.gen_range(0..nations.len());
                Input::ApplyHit(nations[nation_index].0)
            }
            _ => panic!("Unexpected phase: {:?}", self.phase),
        }
    }

    fn out(&self) -> Vec<Output> {
        vec![]
    }
}

#[cfg(test)]
mod robot_tests {
    use super::*;
    use crate::{
        all_technology_types, fixtures::EngineBuilder, io::Output, NationState, TechnologyType,
    };

    #[test]
    fn choose_pr_to_spend_for_initiative_uniformly_within_available_resources_with_a_maximum_of_3()
    {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::ChooseInitiative);

        let input = robot.input();

        assert_eq!(Input::Number(3), input);
    }

    #[test]
    fn choose_random_tech_to_improve() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::ImproveTechnologies(all_technology_types()));

        assert_eq!(Input::Select(TechnologyType::Defense, 4), robot.input());
    }

    #[test]
    fn choose_random_tech_from_available_set_to_improve() {
        let engine = EngineBuilder::new(11)
            .with_resources(Side::Empires, 5)
            .on_turn(2)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::ImproveTechnologies(vec![TechnologyType::Attack]));

        assert_eq!(Input::Select(TechnologyType::Attack, 4), robot.input());
    }

    #[test]
    fn choose_random_neighbour_for_offensive() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 16);
        let mut all_nations_at_war = engine.state.all_nations_at_war(Side::Empires);
        all_nations_at_war.sort();

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::LaunchOffensive(all_nations_at_war));

        let input = robot.input();

        assert_eq!(Input::Offensive(Nation::Germany, Nation::France, 3), input);
    }

    #[test]
    fn choose_random_nation_with_less_breakdown_than_maximum_to_reinforce() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .with_nation(Nation::Germany, NationState::AtWar(6))
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::ReinforceNations);

        let input = robot.input();

        assert_eq!(Input::Reinforce(Nation::Germany, 2), input);
    }

    #[test]
    fn pass_when_there_is_no_nation_to_reinforce() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::ReinforceNations);

        let input = robot.input();

        assert_eq!(Input::Pass, input);
    }

    #[test]
    fn never_commits_or_for_u_boot() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::IncreaseUBoot);

        let input = robot.input();

        assert_eq!(Input::Number(0), input);
    }

    #[test]
    fn never_commits_or_for_blockade() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Allies, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Allies, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::IncreaseBlockade);

        let input = robot.input();

        assert_eq!(Input::Number(0), input);
    }

    #[test]
    fn choose_nation_with_maximum_breakdown_when_applying_hits() {
        let engine = EngineBuilder::new(14)
            .with_nation(Nation::Germany, NationState::AtWar(6))
            .with_nation(Nation::AustriaHungary, NationState::AtWar(2))
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::SelectNationForHit);

        let input = robot.input();

        assert_eq!(Input::ApplyHit(Nation::GermanAfrica), input);
    }
}
