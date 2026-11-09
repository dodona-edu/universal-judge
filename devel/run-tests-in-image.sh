#!/usr/bin/env bash
# Runs devel/run-tests.sh as root in ${IMAGE:-ghcr.io/dodona-edu/dodona-tested:live}; arguments pass through to pytest.
set -euo pipefail
repo="$(cd "$(dirname "$0")/.." && pwd)"
docker run --rm --platform linux/amd64 --user root \
  --volume "$repo:/universal-judge" \
  --workdir /universal-judge \
  "${IMAGE:-ghcr.io/dodona-edu/dodona-tested:live}" \
  bash -c 'devel/install_test_deps.sh && devel/run-tests.sh "$@"' bash "$@"
