#!/bin/bash
set -euo pipefail

ROOT="$(dirname "$(dirname "$0")")"

cd "$ROOT"

isort ./tested ./tests
black ./tested ./tests
