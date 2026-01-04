/// Random Walk E2E Tests for Pacific War
///
/// These tests perform deterministic random walks through the game state space:
/// 1. Create a new battle
/// 2. Use a seeded PRNG to make random choices at each decision point
/// 3. Record all state transitions (from_state, action, to_state)
/// 4. Generate a state machine diagram in DOT format
///
/// Run with: cargo test --features e2e --test random_walk_test
///
/// The tests verify:
/// - Deterministic playthrough (same seed = same sequence)
/// - State space exploration
/// - State machine generation

#[cfg(feature = "e2e")]
mod random_walk {
    use fastrand::Rng;
    use scraper::{Html, Selector};
    use std::collections::{HashMap, HashSet};
    use std::fs::File;
    use std::io::Write;

    const BASE_URL: &str = "http://localhost:8000";

    /// Represents a game state for the state machine
    #[derive(Debug, Clone, Hash, Eq, PartialEq)]
    struct GameState {
        phase: String,
        cycle: Option<u8>,
        segment: Option<String>,
        lighting: Option<String>,
    }

    impl GameState {
        fn from_html(html: &str) -> Self {
            let document = Html::parse_document(html);

            // Extract phase from the page
            let phase_selector = Selector::parse("h2").unwrap();
            let phase = document
                .select(&phase_selector)
                .next()
                .map(|el| el.text().collect::<String>().trim().to_string())
                .unwrap_or_else(|| "Unknown".to_string());

            // Try to extract cycle count if in battle cycle
            let cycle = if phase.contains("Battle Cycle") {
                // Try to find cycle count in the page
                let text = document.root_element().text().collect::<String>();
                if text.contains("Cycle") {
                    Some(1) // Simplified - would need better parsing
                } else {
                    None
                }
            } else {
                None
            };

            GameState {
                phase,
                cycle,
                segment: None,
                lighting: None,
            }
        }

        fn to_dot_label(&self) -> String {
            let mut label = self.phase.clone();
            if let Some(c) = self.cycle {
                label.push_str(&format!("\\nCycle {}", c));
            }
            if let Some(ref s) = self.segment {
                label.push_str(&format!("\\n{}", s));
            }
            if let Some(ref l) = self.lighting {
                label.push_str(&format!("\\n{}", l));
            }
            label
        }

        fn to_dot_id(&self) -> String {
            format!(
                "s_{}_{}_{}",
                self.phase.replace(" ", "_").replace(":", ""),
                self.cycle.map(|c| c.to_string()).unwrap_or_default(),
                self.segment.as_ref().map(|s| s.replace(" ", "_")).unwrap_or_default()
            )
        }
    }

    /// Represents an action/transition in the state machine
    #[derive(Debug, Clone)]
    struct Action {
        name: String,
        method: String,
        path: String,
        form_data: Option<HashMap<String, String>>,
    }

    /// State transition for building the state machine
    #[derive(Debug, Clone)]
    struct Transition {
        from_state: GameState,
        action: String,
        to_state: GameState,
    }

    /// State machine builder that tracks all transitions
    struct StateMachine {
        states: HashSet<GameState>,
        transitions: Vec<Transition>,
    }

    impl StateMachine {
        fn new() -> Self {
            StateMachine {
                states: HashSet::new(),
                transitions: Vec::new(),
            }
        }

        fn add_transition(&mut self, from: GameState, action: String, to: GameState) {
            self.states.insert(from.clone());
            self.states.insert(to.clone());
            self.transitions.push(Transition {
                from_state: from,
                action,
                to_state: to,
            });
        }

        fn to_dot(&self) -> String {
            let mut dot = String::from("digraph StateMachine {\n");
            dot.push_str("  rankdir=LR;\n");
            dot.push_str("  node [shape=box, style=rounded];\n\n");

            // Add all states
            for state in &self.states {
                dot.push_str(&format!(
                    "  {} [label=\"{}\"];\n",
                    state.to_dot_id(),
                    state.to_dot_label()
                ));
            }

            dot.push_str("\n");

            // Add all transitions
            for transition in &self.transitions {
                dot.push_str(&format!(
                    "  {} -> {} [label=\"{}\"];\n",
                    transition.from_state.to_dot_id(),
                    transition.to_state.to_dot_id(),
                    transition.action.replace("\"", "\\\"")
                ));
            }

            dot.push_str("}\n");
            dot
        }

        fn save_to_file(&self, filename: &str) -> std::io::Result<()> {
            let mut file = File::create(filename)?;
            file.write_all(self.to_dot().as_bytes())?;
            Ok(())
        }
    }

    /// Extract available actions from an HTML page
    fn extract_actions(battle_id: &str, html: &str, rng: &mut Rng) -> Vec<Action> {
        let document = Html::parse_document(html);
        let mut actions = Vec::new();

        // Look for forms
        let form_selector = Selector::parse("form").unwrap();
        let button_selector = Selector::parse("button[type='submit'], input[type='submit']").unwrap();
        let input_selector = Selector::parse("input").unwrap();
        let select_selector = Selector::parse("select").unwrap();

        for form in document.select(&form_selector) {
            if let Some(action_attr) = form.value().attr("action") {
                let method = form.value().attr("method").unwrap_or("POST").to_uppercase();

                // Get button text for action name
                let action_name = form.select(&button_selector)
                    .next()
                    .map(|btn| {
                        btn.value().attr("value")
                            .or_else(|| Some(btn.text().collect::<String>().as_str()))
                            .unwrap_or("Submit")
                            .trim()
                            .to_string()
                    })
                    .unwrap_or_else(|| "Submit".to_string());

                // Build form data with random selections for select elements and checkboxes
                let mut form_data = HashMap::new();

                // Handle select dropdowns
                for select in form.select(&select_selector) {
                    if let Some(name) = select.value().attr("name") {
                        // Get all options
                        let option_selector = Selector::parse("option").unwrap();
                        let options: Vec<_> = select.select(&option_selector)
                            .filter_map(|opt| opt.value().attr("value"))
                            .collect();

                        if !options.is_empty() {
                            let idx = rng.usize(0..options.len());
                            form_data.insert(name.to_string(), options[idx].to_string());
                        }
                    }
                }

                // Handle regular inputs with values
                for input in form.select(&input_selector) {
                    if let Some(name) = input.value().attr("name") {
                        if let Some(input_type) = input.value().attr("type") {
                            match input_type {
                                "hidden" | "text" => {
                                    if let Some(value) = input.value().attr("value") {
                                        form_data.insert(name.to_string(), value.to_string());
                                    }
                                }
                                "radio" => {
                                    // For radio buttons, randomly select if this is one
                                    if rng.bool() {
                                        if let Some(value) = input.value().attr("value") {
                                            form_data.insert(name.to_string(), value.to_string());
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }

                actions.push(Action {
                    name: action_name,
                    method,
                    path: action_attr.to_string(),
                    form_data: if form_data.is_empty() { None } else { Some(form_data) },
                });
            }
        }

        // Look for links that trigger actions (like "Next")
        let link_selector = Selector::parse("a[href*='/battle/']").unwrap();
        for link in document.select(&link_selector) {
            if let Some(href) = link.value().attr("href") {
                if href.contains("/next") || href.contains("/contact/") {
                    let action_name = link.text().collect::<String>().trim().to_string();
                    if !action_name.is_empty() {
                        actions.push(Action {
                            name: action_name,
                            method: "GET".to_string(),
                            path: href.to_string(),
                            form_data: None,
                        });
                    }
                }
            }
        }

        actions
    }

    /// Select a random action using the seeded RNG
    fn select_random_action(rng: &mut Rng, actions: &[Action]) -> Option<&Action> {
        if actions.is_empty() {
            None
        } else {
            let index = rng.usize(0..actions.len());
            actions.get(index)
        }
    }

    /// Helper to make HTTP requests
    async fn get(path: &str) -> reqwest::Result<reqwest::Response> {
        reqwest::get(format!("{}{}", BASE_URL, path)).await
    }

    async fn post(path: &str, form: &HashMap<&str, &str>) -> reqwest::Result<reqwest::Response> {
        let client = reqwest::Client::new();
        client
            .post(format!("{}{}", BASE_URL, path))
            .form(form)
            .send()
            .await
    }

    /// Extract UUID from redirect location
    fn extract_uuid_from_location(location: &str) -> Option<String> {
        location.split('/').last().map(|s| s.to_string())
    }

    /// Wait for the server to be healthy
    async fn wait_for_server() -> Result<(), Box<dyn std::error::Error>> {
        for i in 0..30 {
            match reqwest::get(format!("{}/", BASE_URL)).await {
                Ok(_) => return Ok(()),
                Err(_) if i < 29 => {
                    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
                }
                Err(e) => return Err(Box::new(e)),
            }
        }
        Err("Server did not become healthy".into())
    }

    /// Perform a single random walk iteration
    async fn random_walk_iteration(
        seed: u64,
        max_steps: usize,
    ) -> Result<StateMachine, Box<dyn std::error::Error>> {
        let mut rng = Rng::with_seed(seed);
        let mut state_machine = StateMachine::new();

        // Step 1: Create a new battle
        let mut form = HashMap::new();
        form.insert("battle_name", "Random Walk Test");
        form.insert("start_date", "1942-05-01");
        form.insert("duration", "21");
        form.insert("operation_player", "Japan");
        form.insert("intelligence_condition", "Intercept");

        let response = post("/battle", &form).await?;
        let location = response
            .headers()
            .get("location")
            .and_then(|h| h.to_str().ok())
            .expect("Should have location header");

        let battle_id = extract_uuid_from_location(location)
            .expect("Should extract UUID from location");

        // Step 2: Perform random walk
        let mut step_count = 0;
        let mut current_html = String::new();

        while step_count < max_steps {
            // Get current state
            let response = get(&format!("/battle/{}", battle_id)).await?;
            current_html = response.text().await?;

            let current_state = GameState::from_html(&current_html);

            // Extract available actions
            let actions = extract_actions(&battle_id, &current_html, &mut rng);

            if actions.is_empty() {
                println!("No more actions available at step {}", step_count);
                break;
            }

            // Select random action
            let action = select_random_action(&mut rng, &actions)
                .expect("Should have at least one action");

            println!(
                "Step {}: {} -> Action: {}",
                step_count, current_state.phase, action.name
            );

            // Execute action
            let response = if action.method == "GET" {
                get(&action.path).await?
            } else {
                // For POST requests, we need to handle form data
                // This is simplified - would need better form handling
                let form_data = action.form_data.as_ref()
                    .map(|data| {
                        data.iter()
                            .map(|(k, v)| (k.as_str(), v.as_str()))
                            .collect::<HashMap<_, _>>()
                    })
                    .unwrap_or_default();

                let client = reqwest::Client::new();
                client
                    .post(format!("{}{}", BASE_URL, action.path))
                    .form(&form_data)
                    .send()
                    .await?
            };

            // Get new state
            let new_html = response.text().await?;
            let new_state = GameState::from_html(&new_html);

            // Record transition
            state_machine.add_transition(
                current_state.clone(),
                action.name.clone(),
                new_state.clone(),
            );

            step_count += 1;

            // Small delay to avoid overwhelming the server
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        }

        Ok(state_machine)
    }

    #[tokio::test]
    async fn test_deterministic_random_walk() -> Result<(), Box<dyn std::error::Error>> {
        wait_for_server().await?;

        let seed = 42u64;
        let max_steps = 20;

        // Run the same seed twice and verify we get the same sequence
        let state_machine1 = random_walk_iteration(seed, max_steps).await?;
        let state_machine2 = random_walk_iteration(seed, max_steps).await?;

        // Both should have the same number of transitions
        assert_eq!(
            state_machine1.transitions.len(),
            state_machine2.transitions.len(),
            "Same seed should produce same number of transitions"
        );

        // Verify each transition matches
        for (i, (t1, t2)) in state_machine1
            .transitions
            .iter()
            .zip(state_machine2.transitions.iter())
            .enumerate()
        {
            assert_eq!(
                t1.from_state, t2.from_state,
                "Transition {} from_state should match",
                i
            );
            assert_eq!(
                t1.action, t2.action,
                "Transition {} action should match",
                i
            );
            assert_eq!(
                t1.to_state, t2.to_state,
                "Transition {} to_state should match",
                i
            );
        }

        println!("Deterministic test passed: {} transitions", state_machine1.transitions.len());

        Ok(())
    }

    #[tokio::test]
    async fn test_multiple_random_walks_and_generate_state_machine() -> Result<(), Box<dyn std::error::Error>> {
        wait_for_server().await?;

        let iterations = 5;
        let max_steps = 30;
        let mut combined_state_machine = StateMachine::new();

        for i in 0..iterations {
            let seed = 100 + i; // Different seed for each iteration
            println!("\n=== Iteration {} (seed: {}) ===", i + 1, seed);

            let state_machine = random_walk_iteration(seed, max_steps).await?;

            // Merge into combined state machine
            for transition in state_machine.transitions {
                combined_state_machine.add_transition(
                    transition.from_state,
                    transition.action,
                    transition.to_state,
                );
            }
        }

        println!(
            "\nCombined state machine: {} states, {} transitions",
            combined_state_machine.states.len(),
            combined_state_machine.transitions.len()
        );

        // Save to DOT file
        let output_file = "pacific_war_state_machine.dot";
        combined_state_machine.save_to_file(output_file)?;
        println!("State machine diagram saved to: {}", output_file);
        println!("Generate PNG with: dot -Tpng {} -o pacific_war_state_machine.png", output_file);

        Ok(())
    }

    #[tokio::test]
    #[ignore] // Run manually with: cargo test --features e2e --test random_walk_test -- --ignored
    async fn test_exhaustive_random_walks() -> Result<(), Box<dyn std::error::Error>> {
        wait_for_server().await?;

        let iterations = 100;
        let max_steps = 50;
        let mut combined_state_machine = StateMachine::new();

        for i in 0..iterations {
            let seed = 1000 + i;
            if i % 10 == 0 {
                println!("Progress: {}/{} iterations", i, iterations);
            }

            let state_machine = random_walk_iteration(seed, max_steps).await?;

            for transition in state_machine.transitions {
                combined_state_machine.add_transition(
                    transition.from_state,
                    transition.action,
                    transition.to_state,
                );
            }
        }

        println!(
            "\nExhaustive exploration: {} states, {} transitions",
            combined_state_machine.states.len(),
            combined_state_machine.transitions.len()
        );

        let output_file = "pacific_war_state_machine_exhaustive.dot";
        combined_state_machine.save_to_file(output_file)?;
        println!("State machine diagram saved to: {}", output_file);

        Ok(())
    }
}
