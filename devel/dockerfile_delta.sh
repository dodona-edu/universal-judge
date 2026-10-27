#!/bin/bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

cd "$ROOT"

report() {
    if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
        tee -a "$GITHUB_STEP_SUMMARY"
    else
        cat
    fi
}

if [ ! -f /dodona-tested.dockerfile ]; then
    echo "The running image ships no /dodona-tested.dockerfile, so there is nothing to compare .devcontainer/dodona-tested.dockerfile against. Run this inside ghcr.io/dodona-edu/dodona-tested:latest." | report
    exit 1
fi

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

if diff -u --label 'published image /dodona-tested.dockerfile' --label '.devcontainer/dodona-tested.dockerfile' /dodona-tested.dockerfile .devcontainer/dodona-tested.dockerfile > "$TMP/dockerfile.diff"; then
    echo ".devcontainer/dodona-tested.dockerfile matches the dockerfile of the published image." | report
    exit 0
fi

{
    echo ".devcontainer/dodona-tested.dockerfile does not match the dockerfile of the published image. docker-images owns this file: change it there, publish, then copy it here verbatim."
    echo
    echo '```diff'
    cat "$TMP/dockerfile.diff"
    echo '```'
} | report

exit 1
