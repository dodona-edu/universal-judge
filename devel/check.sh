#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

cd "$ROOT"

ruff check ./tested ./tests
ruff format --check ./tested ./tests
pyright ./tested ./tests
ty check
