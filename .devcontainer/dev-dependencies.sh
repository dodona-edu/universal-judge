#!/bin/bash
set -euo pipefail

# Install dev dependencies
pip install --no-cache-dir --upgrade -r "$(dirname "$0")/../requirements-dev.txt"

# add installed packages to path
cat <<EOF >> /home/runner/.bashrc
export PATH=$PATH:/home/runner/.local/bin
EOF
