import shutil
import sys
from collections.abc import Iterator
from pathlib import Path

import pytest

from tested.configs import create_bundle
from tested.languages.cpp import config as cpp_config
from tested.languages.language import Language
from tested.testsuite import Suite
from tests.manual_utils import assert_valid_output, configuration, execute_config

HEADERS = ["values.h", "values.tpp", "evaluation_result.h"]
OBJECTS = ["values.o", "evaluation_result.o"]


@pytest.fixture
def precompiled_directory(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> Iterator[Path]:
    directory = tmp_path / "static"
    directory.mkdir()
    for name in cpp_config.TEMPLATE_FILES:
        shutil.copy(cpp_config.TEMPLATES_DIRECTORY / name, directory)
    for name in OBJECTS:
        (directory / name).write_bytes(b"")
    (directory / "flags").write_text(" ".join(cpp_config.HELPER_FLAGS) + "\n")

    monkeypatch.setattr(cpp_config, "PRECOMPILED_DIRECTORY", directory)
    cpp_config.precompiled_helpers.cache_clear()
    yield directory
    cpp_config.precompiled_helpers.cache_clear()


def _language(
    tmp_path: Path, pytestconfig: pytest.Config, options: dict | None = None
) -> Language:
    conf = configuration(pytestconfig, "", "cpp", tmp_path, options=options)
    return create_bundle(conf, sys.stdout, Suite()).language


def _command(optimization: str, helpers: list[str]) -> list[str]:
    return [
        "g++",
        "-std=c++20",
        "-Wall",
        optimization,
        *helpers,
        "main.cpp",
        "-Wl,--wrap=main",
        "-o",
        "main",
    ]


def test_precompiled_helpers_found(precompiled_directory: Path):
    assert cpp_config.precompiled_helpers() == precompiled_directory


@pytest.mark.parametrize("name", cpp_config.TEMPLATE_FILES)
def test_precompiled_helpers_must_match_templates(
    name: str, precompiled_directory: Path
):
    with (precompiled_directory / name).open("a") as source:
        source.write("// outdated\n")
    assert cpp_config.precompiled_helpers() is None


@pytest.mark.parametrize("name", cpp_config.TEMPLATE_FILES)
def test_precompiled_helpers_need_all_sources(name: str, precompiled_directory: Path):
    (precompiled_directory / name).unlink()
    assert cpp_config.precompiled_helpers() is None


@pytest.mark.parametrize("name", OBJECTS)
def test_precompiled_helpers_need_both_objects(name: str, precompiled_directory: Path):
    (precompiled_directory / name).unlink()
    assert cpp_config.precompiled_helpers() is None


def test_precompiled_helpers_need_flags_file(precompiled_directory: Path):
    (precompiled_directory / "flags").unlink()
    assert cpp_config.precompiled_helpers() is None


def test_precompiled_helpers_must_match_flags(precompiled_directory: Path):
    (precompiled_directory / "flags").write_text("-std=c++23 -Wall -O0\n")
    assert cpp_config.precompiled_helpers() is None


def test_precompiled_helpers_tolerate_whitespace_in_flags(precompiled_directory: Path):
    (precompiled_directory / "flags").write_text("\n  -std=c++20 -Wall -O0  \n\n")
    assert cpp_config.precompiled_helpers() == precompiled_directory


def test_uses_precompiled_helpers(
    precompiled_directory: Path, tmp_path: Path, pytestconfig: pytest.Config
):
    language = _language(tmp_path, pytestconfig)

    assert language.initial_dependencies() == HEADERS + OBJECTS
    assert language.path_to_dependencies() == [
        precompiled_directory,
        cpp_config.TEMPLATES_DIRECTORY,
    ]
    command, _ = language.compilation(["main.cpp"])
    assert command == _command("-O0", ["evaluation_result.o", "values.o"])


def test_compiles_helpers_without_precompiled_directory(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, pytestconfig: pytest.Config
):
    monkeypatch.setattr(cpp_config, "PRECOMPILED_DIRECTORY", tmp_path / "missing")
    cpp_config.precompiled_helpers.cache_clear()
    try:
        language = _language(tmp_path, pytestconfig)
        assert language.initial_dependencies() == cpp_config.TEMPLATE_FILES
        assert language.path_to_dependencies() == [cpp_config.TEMPLATES_DIRECTORY]
        command, _ = language.compilation(["main.cpp"])
        assert command == _command("-O0", ["evaluation_result.cpp", "values.cpp"])
    finally:
        cpp_config.precompiled_helpers.cache_clear()


def test_compiler_optimizations_disable_precompiled_helpers(
    precompiled_directory: Path, tmp_path: Path, pytestconfig: pytest.Config
):
    language = _language(
        tmp_path, pytestconfig, {"options": {"compiler_optimizations": True}}
    )

    assert cpp_config.precompiled_helpers() == precompiled_directory
    assert language.initial_dependencies() == cpp_config.TEMPLATE_FILES
    assert language.path_to_dependencies() == [cpp_config.TEMPLATES_DIRECTORY]
    command, _ = language.compilation(["main.cpp"])
    assert command == _command("-O3", ["evaluation_result.cpp", "values.cpp"])


# Guards the judge's compile flags against those the dodona-tested image used to build
# the objects (cross-repo contract).
@pytest.mark.cpp
@pytest.mark.skipif(
    cpp_config.precompiled_helpers() is None,
    reason="The image has no precompiled C++ helpers",
)
def test_cpp_with_precompiled_helpers(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "echo-function", "cpp", tmp_path, "one.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
