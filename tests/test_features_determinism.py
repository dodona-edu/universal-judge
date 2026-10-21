"""The "not solvable" message must be the same on every run (#753).

The three feature lists in the message were interpolated straight from Python
sets, whose iteration order follows the per-process randomised string hash:
the same suite produced a different message on each run.
"""

import os
import subprocess
import sys
from pathlib import Path

_ROOT = str(Path(__file__).resolve().parent.parent)

_CODE = """
from types import SimpleNamespace

from tested.features import is_supported


class FakeLanguage:
    config = SimpleNamespace(
        suite=SimpleNamespace(
            get_used_features=lambda: SimpleNamespace(
                constructs={"named_arguments", "evaluators", "assignments"},
                types=set(),
                nested_types=set(),
            )
        )
    )

    @staticmethod
    def supported_constructs():
        return set()


message = is_supported(FakeLanguage())
print(message[0].description)
"""


def _message_for_hash_seed(seed: str) -> str:
    env = dict(os.environ)
    env["PYTHONHASHSEED"] = seed
    env["PYTHONPATH"] = os.pathsep.join(
        [_ROOT] + [env["PYTHONPATH"]] if env.get("PYTHONPATH") else [_ROOT]
    )
    proc = subprocess.run(
        [sys.executable, "-c", _CODE], capture_output=True, text=True, env=env
    )
    assert proc.returncode == 0, proc.stderr
    return proc.stdout


def test_not_solvable_message_is_identical_across_hash_seeds():
    messages = [_message_for_hash_seed(seed) for seed in ("0", "1", "2", "3")]
    assert len(set(messages)) == 1
    assert "This test suite is not solvable in" in messages[0]
    assert "evaluators" in messages[0]
