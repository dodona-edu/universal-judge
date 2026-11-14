"""Compare how two TESTed checkouts parse every test suite in a corpus.

Run through devel/corpus-parse-check.sh. Output files can quote teacher content.

    PYTHONHASHSEED=0 python corpus_parse_check.py parse JUDGE_DIR CORPUS OUT_JSON
    python corpus_parse_check.py compare A_JSON B_JSON
"""

import argparse
import contextlib
import hashlib
import io
import json
import os
import sys
from pathlib import Path
from typing import Any

Results = dict[str, dict[str, Any]]


def find_suites(corpus: Path) -> list[Path]:
    suites = [
        path
        for path in corpus.rglob("*")
        if path.is_file()
        and path.suffix.lower() in (".yaml", ".yml", ".json")
        and "evaluation" in path.relative_to(corpus).parts[:-1]
    ]
    return sorted(suites)


def parse_command(judge_dir: str, corpus: str, out_json: str) -> int:
    # Unpinned hash seeds show differences that are not there
    if "PYTHONHASHSEED" not in os.environ:
        print("error: set PYTHONHASHSEED", file=sys.stderr)
        return 2

    root = os.path.realpath(judge_dir)
    sys.path.insert(0, root)
    import tested

    if os.path.commonpath([os.path.realpath(tested.__file__), root]) != root:
        print(
            f"error: tested imported from {tested.__file__}, not {root}",
            file=sys.stderr,
        )
        return 2
    from tested.dsl.translate_parser import parse_dsl
    from tested.parsing import suite_to_json
    from tested.testsuite import parse_test_suite

    corpus_path = Path(corpus)
    results: Results = {}
    for path in find_suites(corpus_path):
        key = str(path.relative_to(corpus_path))
        try:
            text = path.read_text(encoding="utf-8")
            # TESTed prints parse errors with suite lines in them
            with (
                contextlib.redirect_stdout(io.StringIO()),
                contextlib.redirect_stderr(io.StringIO()),
            ):
                if path.suffix.lower() == ".json":
                    suite = parse_test_suite(text)
                else:
                    suite = parse_dsl(text)
                serialised = json.dumps(
                    json.loads(suite_to_json(suite)), sort_keys=True
                )
            digest = hashlib.sha256(serialised.encode()).hexdigest()
            results[key] = {"ok": True, "hash": digest, "len": len(serialised)}
        except Exception as e:  # noqa: BLE001 - the parsers raise any exception type
            first_line = (str(e).splitlines() or [""])[0][:300]
            results[key] = {"ok": False, "exc": type(e).__name__, "msg": first_line}

    Path(out_json).write_text(json.dumps(results, sort_keys=True), encoding="utf-8")
    ok = sum(r["ok"] for r in results.values())
    print(f"total {len(results)}, ok {ok}, failed {len(results) - ok}")
    return 0


def state(result: dict[str, Any] | None) -> str:
    if result is None:
        return "missing"
    return "ok" if result["ok"] else result["exc"]


def compare_command(a_json: str, b_json: str) -> int:
    a: Results = json.loads(Path(a_json).read_text(encoding="utf-8"))
    b: Results = json.loads(Path(b_json).read_text(encoding="utf-8"))
    differing = sorted(k for k in a.keys() | b.keys() if a.get(k) != b.get(k))
    a_ok = sum(r["ok"] for r in a.values())
    b_ok = sum(r["ok"] for r in b.values())
    print(
        f"{len(a)} vs {len(b)} files, {a_ok} vs {b_ok} ok, {len(differing)} differences"
    )
    for key in differing:
        a_state, b_state = state(a.get(key)), state(b.get(key))
        suffix = "\toutput differs" if a_state == b_state else ""
        print(f"{key}\t{a_state}\t{b_state}{suffix}")
    return 1 if differing else 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare how two TESTed checkouts parse a corpus."
    )
    commands = parser.add_subparsers(dest="command", required=True)
    parse = commands.add_parser("parse", help="parse every suite of a corpus")
    parse.add_argument("judge_dir")
    parse.add_argument("corpus")
    parse.add_argument("out_json")
    compare = commands.add_parser("compare", help="compare two parse results")
    compare.add_argument("a_json")
    compare.add_argument("b_json")
    args = parser.parse_args()
    if args.command == "parse":
        return parse_command(args.judge_dir, args.corpus, args.out_json)
    return compare_command(args.a_json, args.b_json)


if __name__ == "__main__":
    sys.exit(main())
