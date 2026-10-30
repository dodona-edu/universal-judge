#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

cd "$ROOT"

ruff check --select I --fix ./tested ./tests
ruff format ./tested ./tests
