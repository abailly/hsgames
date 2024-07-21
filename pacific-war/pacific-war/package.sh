#!/usr/bin/env bash
# Packages pacific war application for deployment to remote host

PKG="pacific-war"                                    # cargo package name
TARGET="x86_64-unknown-linux-gnu"            # remote target
ASSETS=("Rocket.toml" "templates")  # list of assets to bundle
BUILD_DIR="target/${TARGET}/release"         # cargo build directory

## ensure target toolchain is present
rustup target add $TARGET

## cross-compile
cargo zigbuild --target $TARGET --release

## bundle
tar -cvzf "${PKG}.tar.gz" "${ASSETS[@]}" -C "${BUILD_DIR}" "${PKG}"
