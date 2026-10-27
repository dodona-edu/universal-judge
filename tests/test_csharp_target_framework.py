"""
Tests for how the C# language picks the .NET target framework.
"""

from pathlib import Path

import pytest

from tested.languages.csharp.config import _pick_target_framework


def install_pack(root: Path, version: str, framework: str) -> Path:
    reference = (
        root / "packs" / "Microsoft.NETCore.App.Ref" / version / "ref" / framework
    )
    reference.mkdir(parents=True)
    return reference


def test_only_net10_picks_net10(tmp_path: Path):
    reference = install_pack(tmp_path, "10.0.8", "net10.0")
    framework, folder = _pick_target_framework([tmp_path])
    assert framework.reference_dir == "net10.0"
    assert framework.runtimeconfig == "tested.net10.0.runtimeconfig.json"
    assert "NET10_0" in framework.defines
    assert folder == reference


def test_roots_are_combined(tmp_path: Path):
    reference = install_pack(tmp_path / "a", "10.0.8", "net10.0")
    install_pack(tmp_path / "b", "10.0.4", "net10.0")
    framework, folder = _pick_target_framework([tmp_path / "b", tmp_path / "a"])
    assert framework.reference_dir == "net10.0"
    assert folder == reference


def test_newest_reference_pack_is_used(tmp_path: Path):
    install_pack(tmp_path, "10.0.7", "net10.0")
    reference = install_pack(tmp_path, "10.0.8", "net10.0")
    _, folder = _pick_target_framework([tmp_path])
    assert folder == reference


def test_unsupported_framework_is_ignored(tmp_path: Path):
    install_pack(tmp_path, "8.0.11", "net8.0")
    with pytest.raises(FileNotFoundError, match=r"net10\.0"):
        _pick_target_framework([tmp_path])


def test_nothing_installed_names_supported_frameworks(tmp_path: Path):
    with pytest.raises(FileNotFoundError, match=r"net10\.0"):
        _pick_target_framework([tmp_path])
