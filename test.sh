#!/bin/bash
set -ef pipefail
BASE_DIR="$(dirname "$0")"

cargo build -j"$(nproc)"

"$BASE_DIR"/tests/splat/test.sh "$BASE_DIR"/tools/splat/splat "$BASE_DIR/target/debug/file-lang run"
