#!/bin/bash
set -euo pipefail

ROOT="$(dirname "$(dirname "$0")")"

cd "$ROOT"

pytest -n auto \
    --cov=tested \
    --cov-report xml \
    tests/ \
    "$@"
