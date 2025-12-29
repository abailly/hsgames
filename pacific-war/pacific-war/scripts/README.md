# Scripts

This directory contains utility scripts for Pacific War development and testing.

## Available Scripts

### `run-e2e-tests.sh`
Runs the complete end-to-end test suite using containers.

**Features:**
- Auto-detects Docker or Podman
- Builds application container
- Starts PostgreSQL and app services
- Runs all e2e tests
- Automatic cleanup

**Usage:**
```bash
./scripts/run-e2e-tests.sh
```

**Requirements:**
- Docker or Podman installed
- Rust toolchain
- curl

### `setup-macos-containers.sh`
Interactive setup script for macOS users to install and configure container runtimes.

**Features:**
- Checks for existing Docker/Podman installations
- Offers guided installation via Homebrew
- Configures Podman machine with optimal settings
- Supports both Docker and Podman

**Usage:**
```bash
./scripts/setup-macos-containers.sh
```

**What it installs:**
- Option 1: Podman + podman-compose (recommended)
- Option 2: Docker Desktop
- Option 3: Both

**Podman Configuration:**
- 2 CPUs
- 4GB RAM
- 50GB disk

## Container Runtime Support

Both scripts support Docker and Podman, with automatic detection:

| Runtime | Command | Compose Tool |
|---------|---------|--------------|
| Docker | `docker` | `docker compose` or `docker-compose` |
| Podman | `podman` | `podman-compose` |

**Detection Priority:**
1. Podman (preferred on macOS)
2. Docker (fallback)

## Making Scripts Executable

If you get permission errors, make the scripts executable:

```bash
chmod +x scripts/*.sh
```

## Troubleshooting

### Script not found
```bash
# Make sure you're in the pacific-war directory
cd pacific-war
ls scripts/  # Should show the scripts

# Run with explicit path
./scripts/run-e2e-tests.sh
```

### Permission denied
```bash
chmod +x scripts/run-e2e-tests.sh
./scripts/run-e2e-tests.sh
```

### Podman machine issues (macOS)
```bash
# Check machine status
podman machine list

# Start machine
podman machine start

# Reset machine if needed
podman machine stop
podman machine rm
podman machine init
podman machine start
```

### Docker not running
```bash
# macOS: Start Docker Desktop from Applications
# Linux: Start Docker daemon
sudo systemctl start docker
```

## Environment Variables

The scripts respect these environment variables:

- `DATABASE_URL` - Override default database connection
- `ROCKET_PORT` - Change application port (default: 8000)
- `RUST_BACKTRACE` - Enable Rust backtraces (set to `1`)

## CI/CD Integration

These scripts can be used in CI/CD pipelines:

**GitHub Actions Example:**
```yaml
- name: Setup containers
  run: |
    cd pacific-war
    ./scripts/setup-macos-containers.sh

- name: Run E2E tests
  run: |
    cd pacific-war
    ./scripts/run-e2e-tests.sh
```

## Adding New Scripts

When adding new scripts:
1. Add shebang: `#!/bin/bash`
2. Set error handling: `set -e`
3. Add usage documentation
4. Make executable: `chmod +x scripts/your-script.sh`
5. Update this README
