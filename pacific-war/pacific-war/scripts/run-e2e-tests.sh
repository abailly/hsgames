#!/bin/bash

# Exit on error
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Starting Pacific War E2E Tests${NC}"
echo ""

# Detect container runtime and compose command
CONTAINER_CMD=""
COMPOSE_CMD=""

# Check for Podman first (preferred on macOS)
if command -v podman &> /dev/null; then
    CONTAINER_CMD="podman"
    if command -v podman-compose &> /dev/null; then
        COMPOSE_CMD="podman-compose"
        echo -e "${BLUE}Using Podman with podman-compose${NC}"
    else
        echo -e "${YELLOW}Warning: podman found but podman-compose not installed${NC}"
        echo -e "${YELLOW}Install with: brew install podman-compose${NC}"
    fi
fi

# Fall back to Docker if Podman not available or no compose
if [ -z "$COMPOSE_CMD" ]; then
    if command -v docker &> /dev/null; then
        CONTAINER_CMD="docker"
        if docker compose version &> /dev/null 2>&1; then
            COMPOSE_CMD="docker compose"
            echo -e "${BLUE}Using Docker with docker compose plugin${NC}"
        elif command -v docker-compose &> /dev/null; then
            COMPOSE_CMD="docker-compose"
            echo -e "${BLUE}Using Docker with docker-compose${NC}"
        else
            echo -e "${RED}Error: docker found but no compose tool available${NC}"
            exit 1
        fi
    else
        echo -e "${RED}Error: No container runtime found (docker or podman)${NC}"
        echo -e "${YELLOW}Install options:${NC}"
        echo -e "  - Docker Desktop: https://www.docker.com/products/docker-desktop"
        echo -e "  - Podman: brew install podman podman-compose"
        exit 1
    fi
fi

echo ""

# Cleanup function
cleanup() {
    echo ""
    echo -e "${YELLOW}Cleaning up containers...${NC}"
    $COMPOSE_CMD -f docker-compose.e2e.yml down -v 2>/dev/null || true

    # Optionally remove the package tarball
    # Commented out to allow inspection after failed tests
    # rm -f pacific-war.tar.gz
}

# Register cleanup on exit
trap cleanup EXIT

# Stop any existing containers
echo -e "${YELLOW}Stopping existing containers...${NC}"
$COMPOSE_CMD -f docker-compose.e2e.yml down -v 2>/dev/null || true

# Check if zig is installed (required by cargo-zigbuild)
if ! command -v zig &> /dev/null; then
    echo -e "${YELLOW}zig not found. Installing via Homebrew...${NC}"
    if command -v brew &> /dev/null; then
        brew install zig
    else
        echo -e "${RED}Error: zig is required but Homebrew is not installed${NC}"
        echo -e "${YELLOW}Install zig manually: https://ziglang.org/download/${NC}"
        exit 1
    fi
fi

# Check if cargo-zigbuild is installed
if ! command -v cargo-zigbuild &> /dev/null; then
    echo -e "${YELLOW}cargo-zigbuild not found. Installing...${NC}"
    cargo install cargo-zigbuild
fi

# Build the application package
echo -e "${YELLOW}Building application package with cargo zigbuild...${NC}"
./package.sh

# Verify the package was created
if [ ! -f "pacific-war.tar.gz" ]; then
    echo -e "${RED}Error: pacific-war.tar.gz not found after running package.sh${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Application package built successfully${NC}"
echo ""

# Build and start containers
echo -e "${YELLOW}Building container image and starting services...${NC}"
$COMPOSE_CMD -f docker-compose.e2e.yml up -d --build

# Wait for services to be healthy
echo -e "${YELLOW}Waiting for services to be healthy...${NC}"
MAX_WAIT=90
WAIT_TIME=0

while [ $WAIT_TIME -lt $MAX_WAIT ]; do
    # Check if postgres is accessible
    if $CONTAINER_CMD exec pacific-war-postgres-1 pg_isready -U postgres &> /dev/null 2>&1 || \
       $CONTAINER_CMD exec pacific_war_postgres_1 pg_isready -U postgres &> /dev/null 2>&1; then
        # Check if app is healthy
        if curl -sf http://localhost:8000 > /dev/null 2>&1; then
            echo -e "${GREEN}Services are healthy!${NC}"
            break
        fi
    fi

    echo -n "."
    sleep 2
    WAIT_TIME=$((WAIT_TIME + 2))
done

if [ $WAIT_TIME -ge $MAX_WAIT ]; then
    echo -e "${RED}Services failed to become healthy${NC}"
    echo ""
    echo "Container logs:"
    $COMPOSE_CMD -f docker-compose.e2e.yml logs
    exit 1
fi

echo ""
echo -e "${YELLOW}Running E2E tests...${NC}"
echo ""

# Run the tests
cargo test --features e2e --test e2e_test -- --test-threads=1 --nocapture

# Check test result
if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ All E2E tests passed!${NC}"
    exit 0
else
    echo ""
    echo -e "${RED}✗ E2E tests failed${NC}"
    echo ""
    echo "Application logs:"
    $COMPOSE_CMD -f docker-compose.e2e.yml logs app
    exit 1
fi
