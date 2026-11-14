import os
from pathlib import Path

import pytest

from tested.judge.linter import default_eslint_config, eslint_major_version


def _fake_eslint(root: Path, version: str) -> Path:
    package = root / "lib" / "node_modules" / "eslint"
    (package / "bin").mkdir(parents=True)
    (package / "bin" / "eslint.js").write_text("")
    (package / "bin" / "eslint.js").chmod(0o755)  # shutil.which needs it executable
    (package / "package.json").write_text(f'{{"version": "{version}"}}')
    bin_dir = root / "bin"
    bin_dir.mkdir()
    (bin_dir / "eslint").symlink_to(package / "bin" / "eslint.js")
    return bin_dir


@pytest.mark.parametrize(
    ("version", "major", "config"),
    [
        ("8.57.1", 8, "eslintrc.yml"),
        ("9.0.0", 9, "eslint.config.mjs"),
        ("10.14.0", 10, "eslint.config.mjs"),
    ],
)
def test_eslint_version_selects_config(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    version: str,
    major: int,
    config: str,
):
    monkeypatch.setenv("PATH", str(_fake_eslint(tmp_path, version)))
    assert eslint_major_version() == major
    assert default_eslint_config(Path("judge")) == Path("judge") / config


def test_eslint_missing_uses_legacy_config(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setenv("PATH", os.devnull)
    assert eslint_major_version() is None
    assert default_eslint_config(Path("judge")) == Path("judge/eslintrc.yml")
