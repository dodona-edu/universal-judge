#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

cd "$ROOT"

pytest -n logical \
    --cov=tested \
    --cov-report xml \
    tests/ \
    "$@"
