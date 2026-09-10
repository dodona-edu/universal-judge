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
    echo "The running image ships no /dodona-tested.dockerfile, so it predates the baked dockerfile and there is nothing to compare .devcontainer/dodona-tested.dockerfile against. Expected until a docker-images publish ships an image built from a dodona-tested.dockerfile that carries the COPY dodona-tested.dockerfile /dodona-tested.dockerfile line." | report
    exit 1
fi

if diff -u --label 'image /dodona-tested.dockerfile' --label '.devcontainer/dodona-tested.dockerfile' /dodona-tested.dockerfile .devcontainer/dodona-tested.dockerfile > /tmp/dockerfile.diff; then
    echo ".devcontainer/dodona-tested.dockerfile matches the dockerfile the image was built from." | report
    exit 0
fi

{
    echo ".devcontainer/dodona-tested.dockerfile does not match the dockerfile the image was built from. The two copies must be kept identical, with the dodona-edu/docker-images one being what production actually runs: mirror a change here to dodona-tested.dockerfile on docker-images' dev branch, and mirror a change there back here."
    echo
    echo '```diff'
    cat /tmp/dockerfile.diff
    echo '```'
} | report

exit 1
