#!/usr/bin/env bash
# Generates a coverage report from https://github.com/mozilla/grcov
set -e

export CARGO_INCREMENTAL=0
export RUSTFLAGS="-Zprofile -Ccodegen-units=1 -Copt-level=0 -Clink-dead-code -Coverflow-checks=off -Zpanic_abort_tests -Cpanic=abort"
export RUSTDOCFLAGS="-Cpanic=abort"
rustup run nightly cargo build
rustup run nightly cargo test
rustup run nightly grcov ../target/debug/ -s . -t html --llvm --branch --ignore-not-existing -o ../target/debug/coverage/
echo "Created coverage files at ../target/debug/coverage"
