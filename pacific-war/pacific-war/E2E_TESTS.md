# End-to-End Tests

This directory contains container-based end-to-end tests for Pacific War.

## Prerequisites

### Container Runtime
You need either Docker or Podman installed:

**macOS Users:**
```bash
# Quick setup (installs and configures container runtime)
./scripts/setup-macos-containers.sh

# Or manually install Podman (recommended - lightweight, free)
brew install podman podman-compose
podman machine init
podman machine start

# Or install Docker Desktop
brew install --cask docker
# Then start Docker Desktop from Applications
```

**Linux Users:**
```bash
# Docker
sudo apt-get install docker.io docker-compose

# Or Podman
sudo apt-get install podman podman-compose
```

### Other Requirements
- Rust toolchain (install via https://rustup.rs)
- Zig compiler (for cross-compilation via cargo-zigbuild)
  - macOS: `brew install zig`
  - Linux: Download from https://ziglang.org/download/
- cargo-zigbuild (auto-installed by script if missing)
  - `cargo install cargo-zigbuild`
- curl (for health checks)

## Running the Tests

### Quick Start

```bash
./scripts/run-e2e-tests.sh
```

This script will:
1. Auto-detect available container runtime (Podman or Docker)
2. Install zig and cargo-zigbuild if needed
3. Run `package.sh` to cross-compile the app for Linux
4. Build the container image using the pre-built binary
5. Start PostgreSQL and the application in containers
6. Wait for services to be healthy
7. Run all e2e tests
8. Clean up containers and volumes

**Note:**
- The script uses `cargo zigbuild` to cross-compile for Linux, avoiding the need to build Rust inside Docker
- This is much faster than building from source in the container
- The script automatically detects and uses either Docker or Podman
- Podman is preferred on macOS as it's lightweight and doesn't require Docker Desktop licensing

### Manual Steps

If you prefer to run the tests manually:

```bash
# Build the application package
./package.sh

# Start the services (Docker)
docker compose -f docker-compose.e2e.yml up -d --build

# OR with Podman
podman-compose -f docker-compose.e2e.yml up -d --build

# Wait for services to be healthy
# Check with: docker compose ps
# Or: podman-compose ps

# Run the tests
cargo test --features e2e --test e2e_test

# Clean up
docker compose -f docker-compose.e2e.yml down -v
# Or: podman-compose -f docker-compose.e2e.yml down -v
```

## What the Tests Cover

The e2e tests verify:

1. **Battle Creation**: Creating a new battle through the HTTP API
2. **State Mutations**: Making moves and progressing through phases
3. **Persistence**: Data persists correctly in PostgreSQL
4. **Version Handling**: Optimistic locking works across operations
5. **Error Handling**: Invalid UUIDs return proper 404 responses

## Test Structure

- `tests/e2e_test.rs` - Contains all e2e test cases
- `docker-compose.e2e.yml` - Defines the test environment
- `Dockerfile` - Builds the application container
- `scripts/run-e2e-tests.sh` - Convenience script to run tests

## Debugging Failed Tests

If tests fail, you can check the logs:

```bash
# Using Docker
docker compose -f docker-compose.e2e.yml logs app        # Application logs
docker compose -f docker-compose.e2e.yml logs postgres   # Database logs
docker compose -f docker-compose.e2e.yml exec postgres psql -U postgres -d pacific_war_test

# Using Podman
podman-compose -f docker-compose.e2e.yml logs app
podman-compose -f docker-compose.e2e.yml logs postgres
podman-compose -f docker-compose.e2e.yml exec postgres psql -U postgres -d pacific_war_test

# Or use the container runtime directly
docker ps                  # List running containers
podman ps                  # List running containers
docker logs <container>    # View specific container logs
podman logs <container>    # View specific container logs
```

### Common Issues on macOS

**Podman machine not started:**
```bash
podman machine start
podman machine list  # Verify it's running
```

**Port conflicts:**
```bash
# Check if ports 5432 or 8000 are already in use
lsof -i :5432
lsof -i :8000

# Stop conflicting services or change ports in docker-compose.e2e.yml
```

**Build failures:**
```bash
# Clear build cache
docker system prune -a  # Docker
podman system prune -a  # Podman
```

## Writing New Tests

To add a new e2e test:

1. Add a new test function in `tests/e2e_test.rs`
2. Mark it with `#[tokio::test]`
3. Use the helper functions (`get`, `post`, `wait_for_server`)
4. Make assertions about HTTP responses and HTML content

Example:

```rust
#[tokio::test]
async fn test_my_new_feature() -> Result<(), Box<dyn std::error::Error>> {
    wait_for_server().await?;

    // Your test logic here
    let response = get("/some/path").await?;
    assert_eq!(response.status(), reqwest::StatusCode::OK);

    Ok(())
}
```

## CI/CD Integration

The e2e tests can be integrated into CI/CD pipelines. Example GitHub Actions workflow:

```yaml
name: E2E Tests

on: [push, pull_request]

jobs:
  e2e:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
      - name: Run E2E Tests
        run: |
          cd pacific-war
          ./scripts/run-e2e-tests.sh
```

## Performance

The e2e tests typically take 30-60 seconds to run, including:
- Building the Docker image (~20-30s)
- Starting services (~5-10s)
- Running tests (~5-10s)
- Cleanup (~2-5s)

Subsequent runs may be faster due to Docker layer caching.
