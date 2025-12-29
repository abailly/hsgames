#!/bin/bash

# Setup script for container runtime on macOS
# This script helps install and configure Podman or Docker on macOS

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Pacific War Container Runtime Setup for macOS ===${NC}"
echo ""

# Check if Homebrew is installed
if ! command -v brew &> /dev/null; then
    echo -e "${RED}Error: Homebrew is not installed${NC}"
    echo -e "${YELLOW}Install Homebrew first: https://brew.sh${NC}"
    echo ""
    echo "Run: /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
    exit 1
fi

echo -e "${GREEN}✓ Homebrew is installed${NC}"
echo ""

# Check what's already installed
HAS_DOCKER=false
HAS_PODMAN=false

if command -v docker &> /dev/null; then
    HAS_DOCKER=true
    echo -e "${GREEN}✓ Docker is already installed${NC}"
    docker --version
fi

if command -v podman &> /dev/null; then
    HAS_PODMAN=true
    echo -e "${GREEN}✓ Podman is already installed${NC}"
    podman --version
fi

echo ""

# If both are installed, we're done
if [ "$HAS_DOCKER" = true ] && [ "$HAS_PODMAN" = true ]; then
    echo -e "${GREEN}Both Docker and Podman are installed!${NC}"
    echo -e "${YELLOW}The e2e test script will automatically detect and use the appropriate tool.${NC}"
    exit 0
fi

# If neither is installed, offer choices
if [ "$HAS_DOCKER" = false ] && [ "$HAS_PODMAN" = false ]; then
    echo -e "${YELLOW}No container runtime detected. Choose an option:${NC}"
    echo ""
    echo "1) Install Podman (Recommended - lightweight, no daemon, free)"
    echo "2) Install Docker Desktop (Requires license for commercial use)"
    echo "3) Install both"
    echo "4) Exit and install manually"
    echo ""
    read -p "Enter choice [1-4]: " choice

    case $choice in
        1)
            echo ""
            echo -e "${BLUE}Installing Podman...${NC}"
            brew install podman podman-compose

            echo ""
            echo -e "${BLUE}Initializing Podman machine...${NC}"
            podman machine init --cpus 2 --memory 4096 --disk-size 50
            podman machine start

            echo ""
            echo -e "${GREEN}✓ Podman installed and configured!${NC}"
            ;;
        2)
            echo ""
            echo -e "${BLUE}Installing Docker Desktop...${NC}"
            brew install --cask docker

            echo ""
            echo -e "${YELLOW}Please start Docker Desktop from Applications${NC}"
            echo -e "${YELLOW}Then run this setup script again to verify${NC}"
            ;;
        3)
            echo ""
            echo -e "${BLUE}Installing both Podman and Docker...${NC}"
            brew install podman podman-compose
            brew install --cask docker

            echo ""
            echo -e "${BLUE}Initializing Podman machine...${NC}"
            podman machine init --cpus 2 --memory 4096 --disk-size 50
            podman machine start

            echo ""
            echo -e "${GREEN}✓ Podman installed and configured!${NC}"
            echo -e "${YELLOW}Please start Docker Desktop from Applications${NC}"
            ;;
        4)
            echo ""
            echo -e "${YELLOW}Installation options:${NC}"
            echo "  Podman: brew install podman podman-compose"
            echo "  Docker: brew install --cask docker"
            exit 0
            ;;
        *)
            echo -e "${RED}Invalid choice${NC}"
            exit 1
            ;;
    esac
fi

# If only Docker is installed, offer to install Podman
if [ "$HAS_DOCKER" = true ] && [ "$HAS_PODMAN" = false ]; then
    echo -e "${YELLOW}Docker is installed. Would you also like to install Podman? (y/n)${NC}"
    read -p "> " install_podman

    if [ "$install_podman" = "y" ] || [ "$install_podman" = "Y" ]; then
        echo ""
        echo -e "${BLUE}Installing Podman...${NC}"
        brew install podman podman-compose

        echo ""
        echo -e "${BLUE}Initializing Podman machine...${NC}"
        podman machine init --cpus 2 --memory 4096 --disk-size 50
        podman machine start

        echo ""
        echo -e "${GREEN}✓ Podman installed and configured!${NC}"
    fi
fi

# If only Podman is installed, offer to install Docker
if [ "$HAS_PODMAN" = true ] && [ "$HAS_DOCKER" = false ]; then
    echo -e "${YELLOW}Podman is installed. Would you also like to install Docker Desktop? (y/n)${NC}"
    read -p "> " install_docker

    if [ "$install_docker" = "y" ] || [ "$install_docker" = "Y" ]; then
        echo ""
        echo -e "${BLUE}Installing Docker Desktop...${NC}"
        brew install --cask docker

        echo ""
        echo -e "${YELLOW}Please start Docker Desktop from Applications${NC}"
    fi
fi

echo ""
echo -e "${GREEN}=== Setup Complete ===${NC}"
echo ""
echo -e "${BLUE}Next steps:${NC}"
echo "1. Run the e2e tests: ./scripts/run-e2e-tests.sh"
echo "2. The script will automatically detect and use the appropriate container runtime"
echo ""

# If Podman was installed, show how to manage the machine
if command -v podman &> /dev/null; then
    echo -e "${BLUE}Podman commands:${NC}"
    echo "  podman machine start  - Start the Podman VM"
    echo "  podman machine stop   - Stop the Podman VM"
    echo "  podman machine list   - Show Podman machines"
    echo ""
fi
