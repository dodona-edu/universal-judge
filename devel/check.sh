#!/bin/bash
set -euo pipefail

ROOT="$(dirname "$(dirname "$0")")"

cd "$ROOT"

isort --check-only --diff ./tested ./tests
black --check ./tested ./tests
pyright ./tested ./tests
