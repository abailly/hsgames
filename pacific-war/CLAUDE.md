# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Pacific War is a web-based battle simulation implementing the complex combat rules from the GMT Games "Pacific War: The Forgotten War" board game. The application models multi-phase naval battles in the Pacific Theater of WWII.

**Tech Stack:**
- Rust backend using Rocket web framework (v0.5.1)
- Handlebars templates for server-side rendering
- WebAssembly support via wasm-bindgen (currently minimal)
- Webpack bundler for WASM integration

## Build Commands

### Rust Backend
```bash
# Build the Rust application
cd pacific-war
cargo build

# Build release version
cargo build --release

# Run the server (requires DATABASE_URL environment variable)
DATABASE_URL=postgres://localhost/pacific_war cargo run

# Run unit tests (skips postgres tests if DATABASE_URL not set)
cargo test

# Run unit tests with postgres tests
DATABASE_URL=postgres://localhost/pacific_war_test cargo test

# Run end-to-end tests (requires Docker)
./scripts/run-e2e-tests.sh
```

### Database Setup
```bash
# Create database
createdb pacific_war

# Database migrations run automatically on server startup via sqlx::migrate!

# For testing
createdb pacific_war_test
```

### Deployment Packaging
```bash
# Cross-compile for Linux deployment using cargo zigbuild
./package.sh

# This creates pacific-war.tar.gz containing:
# - Compiled binary (x86_64-unknown-linux-gnu)
# - static/ directory
# - templates/ directory
# - Rocket.toml

# Requirements:
brew install zig              # macOS
cargo install cargo-zigbuild
```

### WebAssembly Build
```bash
# Build WASM package (outputs to pacific-war/pkg/)
cd pacific-war
wasm-pack build --target bundler

# Run WASM browser tests
wasm-pack test --headless --firefox
```

### Frontend Development
```bash
# Install dependencies
cd www
npm install

# Build webpack bundle
npm run build

# Run webpack dev server
npm start
```

## Core Architecture

### Game State Model (src/core.rs)

The game implements a hierarchical state machine with the following key types:

**Battle Flow:**
1. `NewBattle` - Initial battle creation with parameters (date, duration, sides, intelligence condition)
2. `ContactPhase` - Operation/Reaction movement phases (Ground, Air, Naval)
3. `BattleCyclePhase` - Main combat cycles with 16 distinct segments

**Key Enums:**
- `Phase` - Top-level phase: OperationContactPhase, ReactionContactPhase, BattleCyclePhase
- `BattleCycleSegment` - 16 battle phases: SetLighting, AdvantageDetermination, AdvantageMovement, DisadvantageMovement, AirMissionPhases (5 types), NavalCombats, Bombardment, Demolition, Rally, ActivationDeactivation, DetectionRemoval
- `Lighting` - Day-AM, Day-PM, Dusk, Night (affects background visuals)
- `Intelligence` - Surprise, Intercept, Ambush, Unknown (affects combat flow)
- `Side` - Japan vs Allies
- `MovementType` - Ground, Air, Naval

**State Persistence:**
- PostgreSQL-backed persistence with trait-based abstraction
- Repository pattern: `BattleRepository` trait with multiple implementations
  - `PostgresRepository` - Production persistence with JSONB storage
  - `CachedRepository` - Write-through cache wrapper for performance
  - `InMemoryRepository` - For testing
- Optimistic locking with version numbers to prevent concurrent modifications
- Battle state persists across server restarts
- Migrations managed via sqlx

### HTTP Server (src/main.rs)

**Routes:**
- `GET /` - Battle creation form
- `POST /battle` - Create new battle
- `GET /battle/<id>` - Display battle state
- `POST /battle/<id>/contact/<movement>` - Process contact phase movement
- `POST /battle/<id>/battle_cycle/*` - Various battle cycle actions

**Template Rendering:**
All views use Rocket's `Template::render()` with Handlebars templates in `templates/` directory.

### Template Structure

```
templates/
├── index.html.hbs              # Battle creation form
├── battle.html.hbs             # Master battle display
├── contact.html.hbs            # Contact phase UI
└── battle_cycle/
    ├── lighting.html.hbs       # Lighting selection
    ├── advantage_determination.html.hbs
    ├── movement.html.hbs
    ├── naval_combat*.html.hbs  # Naval combat phases
    ├── bombardment.html.hbs
    ├── demolition.html.hbs
    ├── rally.html.hbs
    ├── deactivation.html.hbs
    └── *_rules.html.hbs        # Parallel rule explanation templates
```

### Combat System Details

**Battle Cycles:**
- Multiple cycles per battle (determined by duration)
- Each cycle has lighting conditions that affect combat
- Advantage determination uses die rolls with intelligence modifiers
- Movement phases offer randomized movement options
- Naval combat includes detection states and distance bidding

**Naval Combat:**
- Detection states for both Operation and Reaction sides
- Combat distance determination (close range to long range)
- Hex type affects combat (Open Water, Shoal, Port, etc.)
- Round-by-round resolution

### Styling

**CSS:** Single stylesheet at `static/pacific.css`

**Background Images:**
- `static/day-am.jpeg`, `day-pm.jpeg`, `dusk.jpeg`, `night.jpeg`
- `static/battle_bw.jpeg` - Grayscale battle background
- Dynamic background switching based on `Lighting` enum value

**Design:**
- Mobile-friendly responsive design
- Accordion-style collapsible battle header
- Google Fonts: Nunito Sans

## Testing Approach

**Unit Tests:**
Located in `src/main.rs` and `src/core.rs` using Rocket's async test client:
```rust
let client = Client::tracked(test_rocket()).await.expect("valid rocket instance");
```

**Test Coverage:**
- Lighting selection rules
- Advantage determination calculations
- Phase transitions
- Combat distance logic
- Persistence layer (repository tests)
- Optimistic locking and concurrent modification detection

**End-to-End Tests:**
Container-based integration tests in `tests/e2e_test.rs`:
- Full HTTP API testing against real PostgreSQL
- Battle creation and state progression
- Persistence verification across operations
- Version handling and error cases

**Running E2E Tests:**
```bash
./scripts/run-e2e-tests.sh
```
See `E2E_TESTS.md` for detailed documentation.

**Database Tests:**
PostgreSQL-specific tests run conditionally based on `DATABASE_URL` environment variable:
```bash
DATABASE_URL=postgres://localhost/pacific_war_test cargo test
```

**WASM Tests:**
Browser-based tests in `tests/web.rs` using `wasm-bindgen-test`.

**Playwright Random Walk Tests:**
TypeScript-based browser automation tests in `e2e-playwright/`:
- Deterministic random walks through game UI using seeded PRNG
- State machine generation in DOT/JSON formats
- Multiple iteration modes: 20, 50, or 100 walks
- Full browser automation (clicks, forms) vs HTTP-only testing

```bash
# Setup
cd e2e-playwright
npm install
npx playwright install chromium

# Run tests (server must be running)
npm run test:deterministic  # Quick deterministic test
npm run test:20            # 20 iterations (3 min timeout)
npm run test:50            # 50 iterations (5 min timeout)
npm run test:100           # 100 iterations (10 min timeout)

# Or with custom timeout (in seconds)
TIMEOUT=300 npm test -- --grep "50 iterations"

# Generate diagrams from output
dot -Tpng output/state-machine-20.dot -o output/state-machine-20.png
```

See `e2e-playwright/README.md` and `e2e-playwright/QUICKSTART.md` for details.

Also see `pacific-war/tests/random_walk_test.rs` for Rust HTTP-based random walk tests.

## Development Patterns

**Adding New Battle Phases:**
1. Add variant to `BattleCycleSegment` enum in `src/core.rs`
2. Update `next_phase()` logic in battle cycle progression
3. Create corresponding template in `templates/battle_cycle/`
4. Add route handler in `src/main.rs` if interactive
5. Add CSS classes for any phase-specific styling

**State Transitions:**
All state changes go through methods on `Battle` struct. Key methods:
- `new()` - Create battle from form data
- `start_contact_phase()` - Begin contact phase
- `process_contact_movement()` - Handle movement selection
- `start_battle_cycle()` - Initialize battle cycle
- `advance_cycle()` - Progress to next cycle phase

**Template Rendering:**
All templates receive full battle state context. Common pattern:
```rust
Template::render("template_name", context! {
    battle: &battle,
    /* phase-specific data */
})
```

## Claude Skill: Strategic Advisor

The `.claude/skills/pacific-war/` directory contains a Claude skill for strategic game planning (not game AI). This skill:
- Analyzes Pacific War game situations
- Provides strategic planning assistance
- Offers "what-if" scenario analysis
- Assumes user provides game state and rules knowledge

See `.claude/skills/pacific-war/README.md` for usage examples.

## Important Constraints

**No Persistence:**
Battle state exists only in server memory. Implement serialization/database if persistence needed.

**WASM Underutilized:**
`src/lib.rs` contains minimal WASM bindings. The WASM package is built but not fully integrated into the web frontend.

**Form-Based UI:**
Currently uses traditional form submissions. Consider adding JavaScript interactivity for richer UX.

**Single Server Instance:**
In-memory state means no horizontal scaling. Use shared state (Redis, database) for multi-instance deployment.
