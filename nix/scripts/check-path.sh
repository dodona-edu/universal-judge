#!/usr/bin/env bash
# Fail if the set of commands on PATH in an image drifts from the checked-in
# snapshot. Run after loading the images:
#   nix/scripts/check-path.sh tested-bash inventory/expected-tested-bash.txt
# Pass --update to regenerate the snapshot.
set -euo pipefail

image="${1:?usage: check-path.sh <image> <expected-file> [--update]}"
expected="${2:?usage: check-path.sh <image> <expected-file> [--update]}"
mode="${3:-check}"

actual="$(docker run --rm --user runner "${image}:nix" bash -c 'compgen -c' | LC_ALL=C sort -u)"

if [[ "$mode" == "--update" ]]; then
  printf '%s\n' "$actual" > "$expected"
  echo "updated $expected"
  exit 0
fi

if ! diff -u "$expected" <(printf '%s\n' "$actual"); then
  echo "PATH contents of $image changed. Review the diff; if intended, rerun with --update." >&2
  exit 1
fi
echo "$image PATH matches $expected"
