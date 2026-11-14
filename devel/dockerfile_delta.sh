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
    echo "The running image ships no /dodona-tested.dockerfile, so there is nothing to compare .devcontainer/dodona-tested.dockerfile against. Run this inside ghcr.io/dodona-edu/dodona-tested:live." | report
    exit 1
fi

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

FAILED=0

# mismatch <image path> <mirror path> <diff file>
mismatch() {
    local image_path="$1" mirror_path="$2" diff_file="$3"
    FAILED=1
    {
        echo "$mirror_path does not match $image_path of the live image."
        echo
        echo '```diff'
        cat "$diff_file"
        echo '```'
        echo
    } | report
}

check_file() {
    local image_path="$1" mirror_path="$2"
    if [ ! -f "$image_path" ]; then
        echo "$image_path is missing from the live image, but $mirror_path exists." > "$TMP/file.diff"
    elif [ ! -f "$mirror_path" ]; then
        echo "$mirror_path is missing, but the live image ships $image_path." > "$TMP/file.diff"
    elif diff -u --label "live image $image_path" --label "$mirror_path" "$image_path" "$mirror_path" > "$TMP/file.diff"; then
        echo "$mirror_path matches $image_path of the live image." | report
        return
    fi
    mismatch "$image_path" "$mirror_path" "$TMP/file.diff"
}

check_dir() {
    local image_path="$1" mirror_path="$2"
    if [ ! -d "$image_path" ] && [ ! -d "$mirror_path" ]; then
        return
    fi
    if [ ! -d "$image_path" ]; then
        echo "$image_path is missing from the live image, but $mirror_path exists." > "$TMP/dir.diff"
    elif [ ! -d "$mirror_path" ]; then
        echo "$mirror_path is missing, but the live image ships $image_path." > "$TMP/dir.diff"
    elif diff -ru "$image_path" "$mirror_path" > "$TMP/dir.diff"; then
        echo "$mirror_path matches $image_path of the live image." | report
        return
    fi
    mismatch "$image_path" "$mirror_path" "$TMP/dir.diff"
}

check_file /dodona-tested.dockerfile .devcontainer/dodona-tested.dockerfile
check_file /main.sh .devcontainer/main.sh
check_dir /requirements .devcontainer/dodona-tested

if [ "$FAILED" -ne 0 ]; then
    echo "docker-images owns these files: change them there. Its mark-live workflow opens a sync PR (branch chore/sync-dodona-tested) on every go-live; merge that instead of editing the mirror by hand." | report
    exit 1
fi
