use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

use crate::io::{Input, Output, Player};
use crate::{GameState, Nation, Side, Technology};

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
            Output::ImproveTechnologies => {
                self.phase = Some(message.clone());
            }
            Output::LaunchOffensive => {
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
            Output::OffensiveResult { from, to, result } => {}
            Output::IncreaseUBoot => {}
            Output::UBootResult(_) => {}
            Output::IncreaseBlockade => {}
            Output::BlockadeResult(_) => {}
            Output::SelectNationForHit => {}
            Output::EventDrawn(_, _) => {}
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
            Some(Output::ImproveTechnologies) => {
                let state = self.state.as_ref().unwrap();
                let resources = state.state_of_war.get(&self.side).unwrap().resources;
                let max_pr = resources.min(3);
                let techs: Vec<Technology> = state.available_technologies(&self.side);
                let pr = self.rng.gen_range(0..=max_pr);
                if pr > 0 && !techs.is_empty() {
                    let tech = &techs[self.rng.gen_range(0..techs.len())];
                    Input::Select(tech.category, pr)
                } else {
                    Input::Pass
                }
            }
            Some(Output::LaunchOffensive) => {
                let state = self.state.as_ref().unwrap();
                let resources = state.state_of_war.get(&self.side).unwrap().resources;
                // select a source for offensive
                let mut sources: Vec<Nation> = state.all_nations_at_war(self.side);
                sources.sort();
                let source_index = self.rng.gen_range(0..sources.len());
                println!("sources: {:?} / {}", source_index, sources.len());
                let source = sources[source_index];
                // select a target for offensive
                let mut targets: Vec<&Nation> = state.neighbours(&source);
                targets.sort();
                let target_index = self.rng.gen_range(0..targets.len());
                println!("target: {:?} / {}", target_index, targets.len());
                let target = targets[target_index];
                if resources > 0 {
                    let pr = self.rng.gen_range(0..=resources.min(3));
                    println!("pr: {:?}", pr);
                    Input::Offensive(source, *target, pr)
                } else {
                    Input::Pass
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
                nations.sort();
                let nation_index = self.rng.gen_range(0..nations.len());
                let (nation, loss) = nations[nation_index];
                let pr = self.rng.gen_range(0..=resources.min(loss));
                Input::Reinforce(nation, pr)
            }
            _ => Input::Next,
        }
    }

    fn out(&self) -> Vec<Output> {
        vec![]
    }
}

#[cfg(test)]
mod robot_tests {
    use super::*;
    use crate::{fixtures::EngineBuilder, io::Output, NationState, TechnologyType};

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
    fn choose_random_techs_to_improve() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::ImproveTechnologies);

        let input = robot.input();

        assert_eq!(Input::Select(TechnologyType::Defense, 3), input);
    }

    #[test]
    fn choose_random_neighbour_for_offensive() {
        let engine = EngineBuilder::new(14)
            .with_resources(Side::Empires, 5)
            .build();

        let mut robot = RobotIO::new(&Side::Empires, 15);

        robot.output(&Output::CurrentState(engine.state));
        robot.output(&Output::LaunchOffensive);

        let input = robot.input();

        assert_eq!(
            Input::Offensive(Nation::OttomanEmpire, Nation::Egypt, 2),
            input
        );
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
}
