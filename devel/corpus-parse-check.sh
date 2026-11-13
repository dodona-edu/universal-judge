#!/usr/bin/env bash
# Usage: devel/corpus-parse-check.sh CORPUS [BASE_REF]
# Parses every suite in CORPUS with BASE_REF (default origin/master) and with the working tree,
# inside ${IMAGE:-ghcr.io/dodona-edu/dodona-tested:live}, and reports suites that parse differently.
# Exits 1 on differences; rerun with PYTHONHASHSEED=1 to confirm a difference.
set -euo pipefail

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
  echo "Usage: $0 CORPUS [BASE_REF]" >&2
  exit 2
fi
[ -d "$1" ] || { echo "error: corpus not found: $1" >&2; exit 2; }
corpus="$(cd "$1" && pwd)"
base_ref="${2:-origin/master}"
repo="$(cd "$(dirname "$0")/.." && pwd)"
image="${IMAGE:-ghcr.io/dodona-edu/dodona-tested:live}"

base="$(mktemp -d)"
# readable for the image's runner user (Docker Desktop on macOS hides missing permissions)
chmod 755 "$base"
trap 'rm -rf "$base"' EXIT
git -C "$repo" archive "$base_ref" | tar -x -C "$base"

# Outside the repo: results can quote teacher content; writable for the image's runner user
out="$(mktemp -d)"
chmod 777 "$out"

run() { # run <judge dir in container> <result name>
  docker run --rm --platform linux/amd64 --network none \
    --env PYTHONHASHSEED="${PYTHONHASHSEED:-0}" \
    --volume "$corpus:/corpus:ro" \
    --volume "$base:/base:ro" \
    --volume "$repo:/candidate:ro" \
    --volume "$out:/out" \
    "$image" python /candidate/devel/corpus_parse_check.py parse "$1" /corpus "/out/$2.json"
}

run /base base & base_pid=$!
run /candidate candidate & candidate_pid=$!
status=0
wait "$base_pid" || status=1
wait "$candidate_pid" || status=1
if [ "$status" -ne 0 ]; then
  echo "error: a parse run failed; output dir: $out" >&2
  exit 1
fi

compare_status=0
docker run --rm --platform linux/amd64 --network none \
  --volume "$repo:/candidate:ro" \
  --volume "$out:/out:ro" \
  "$image" python /candidate/devel/corpus_parse_check.py compare /out/base.json /out/candidate.json \
  || compare_status=$?
echo "Output dir: $out"
exit "$compare_status"
