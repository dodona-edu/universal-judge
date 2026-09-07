#!/bin/bash
set -euo pipefail

ROOT="$(dirname "$(dirname "$0")")"

pip3 install -r "$ROOT/requirements-dev.txt"
