#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

cd "$ROOT"

isort --check-only --diff ./tested ./tests
black --check ./tested ./tests
pyright ./tested ./tests
