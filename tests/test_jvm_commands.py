"""
Tests for the commands the Kotlin and Java languages use to compile and run.
"""

import os
import subprocess
import warnings
from pathlib import Path
from types import SimpleNamespace
from typing import Any, cast

import pytest

from tested.languages.java import config as java_config
from tested.languages.java.config import Java
from tested.languages.kotlin import config as kotlin_config
from tested.languages.kotlin.config import Kotlin


@pytest.fixture(autouse=True)
def clear_kotlin_home_cache():
    kotlin_config._kotlin_home.cache_clear()
    yield
    kotlin_config._kotlin_home.cache_clear()


def fake_kotlin_home(root: Path, with_jar: bool = True) -> Path:
    home = root / "kotlin"
    (home / "bin").mkdir(parents=True)
    (home / "lib").mkdir()
    (home / "bin" / "kotlinc").touch()
    if with_jar:
        (home / "lib" / "kotlin-compiler.jar").touch()
    return home


def kotlin_language() -> Kotlin:
    config = SimpleNamespace(dodona=SimpleNamespace(memory_limit=65536))
    return Kotlin(cast(Any, config))


def find_kotlinc(monkeypatch: pytest.MonkeyPatch, kotlinc: Path | None):
    monkeypatch.setattr(
        kotlin_config.shutil,
        "which",
        lambda _: None if kotlinc is None else str(kotlinc),
    )


def test_kotlin_compilation_uses_java_directly(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    home = fake_kotlin_home(tmp_path)
    find_kotlinc(monkeypatch, home / "bin" / "kotlinc")
    command, _ = Kotlin(None).compilation(["Main.kt", "lib.jar"])
    assert command[0] == "java"
    assert "-Xmx192M" in command
    assert "-XX:+UseSerialGC" in command
    assert f"-Dkotlin.home={home.resolve()}" in command
    assert str(home.resolve() / "lib" / "kotlin-compiler.jar") in command
    assert "org.jetbrains.kotlin.cli.jvm.K2JVMCompiler" in command
    assert command[-1] == "Main.kt"
    assert "lib.jar" not in command


def test_kotlin_execution_uses_java_directly(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    home = fake_kotlin_home(tmp_path).resolve()
    find_kotlinc(monkeypatch, home / "bin" / "kotlinc")
    command = kotlin_language().execution(tmp_path, "MainKt.kt", ["a", "b"])
    classpath = os.pathsep.join(
        [
            ".",
            str(home / "lib" / "kotlin-stdlib.jar"),
            str(home / "lib" / "kotlin-reflect.jar"),
        ]
    )
    assert command[0] == "java"
    assert command[command.index("-cp") + 1] == classpath
    assert command[-3:] == ["MainKt", "a", "b"]


def test_kotlin_falls_back_without_kotlinc(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    find_kotlinc(monkeypatch, None)
    compile_command, _ = Kotlin(None).compilation(["Main.kt"])
    run_command = kotlin_language().execution(tmp_path, "MainKt.kt", [])
    assert compile_command[:2] == ["kotlinc", "-J-Xmx192M"]
    assert run_command[0] == "kotlin"


def test_kotlin_falls_back_without_compiler_jar(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    home = fake_kotlin_home(tmp_path, with_jar=False)
    find_kotlinc(monkeypatch, home / "bin" / "kotlinc")
    compile_command, _ = Kotlin(None).compilation(["Main.kt"])
    run_command = kotlin_language().execution(tmp_path, "MainKt.kt", [])
    assert compile_command[0] == "kotlinc"
    assert run_command[0] == "kotlin"


def test_kotlin_aot_cache_only_when_file_exists(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    home = fake_kotlin_home(tmp_path)
    find_kotlinc(monkeypatch, home / "bin" / "kotlinc")
    cache = tmp_path / "kotlinc.aot"
    monkeypatch.setattr(kotlin_config, "KOTLINC_AOT_CACHE", cache)
    command, _ = Kotlin(None).compilation(["Main.kt"])
    assert not any(part.startswith("-XX:AOTCache") for part in command)
    assert "-Xlog:aot*=off" not in command
    cache.touch()
    command, _ = Kotlin(None).compilation(["Main.kt"])
    assert f"-XX:AOTCache={cache}" in command
    assert "-Xlog:aot*=off" in command


def test_java_aot_cache_only_when_file_exists(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    cache = tmp_path / "javac.aot"
    monkeypatch.setattr(java_config, "JAVAC_AOT_CACHE", cache)
    command, _ = Java(None).compilation(["Main.java"])
    assert "-J-XX:+UseSerialGC" in command
    assert not any(part.startswith("-J-XX:AOTCache") for part in command)
    assert "-J-Xlog:aot*=off" not in command
    cache.touch()
    command, _ = Java(None).compilation(["Main.java"])
    assert f"-J-XX:AOTCache={cache}" in command
    assert "-J-Xlog:aot*=off" in command


# The image and the judge go live separately: a cache that does not fit must be
# ignored, never break a compile or print anything.
@pytest.mark.kotlin
@pytest.mark.parametrize(
    "content", [os.urandom(100_000), b""], ids=["garbage", "empty"]
)
def test_kotlin_compiles_silently_with_a_damaged_aot_cache(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, content: bytes
):
    cache = tmp_path / "kotlinc.aot"
    cache.write_bytes(content)
    monkeypatch.setattr(kotlin_config, "KOTLINC_AOT_CACHE", cache)
    (tmp_path / "Main.kt").write_text('fun main() { println("hi") }\n')
    command, _ = Kotlin(None).compilation(["Main.kt"])
    assert f"-XX:AOTCache={cache}" in command
    result = subprocess.run(
        command, cwd=tmp_path, capture_output=True, text=True, check=False
    )
    assert result.returncode == 0
    assert result.stdout == ""
    assert result.stderr == ""
    assert (tmp_path / "MainKt.class").exists()


@pytest.mark.java
@pytest.mark.parametrize(
    "content", [os.urandom(100_000), b""], ids=["garbage", "empty"]
)
def test_java_compiles_silently_with_a_damaged_aot_cache(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, content: bytes
):
    cache = tmp_path / "javac.aot"
    cache.write_bytes(content)
    monkeypatch.setattr(java_config, "JAVAC_AOT_CACHE", cache)
    (tmp_path / "Main.java").write_text(
        "public class Main { public static void main(String[] a) {} }\n"
    )
    command, _ = Java(None).compilation(["Main.java"])
    assert f"-J-XX:AOTCache={cache}" in command
    result = subprocess.run(
        command, cwd=tmp_path, capture_output=True, text=True, check=False
    )
    assert result.returncode == 0
    assert result.stdout == ""
    assert result.stderr == ""
    assert (tmp_path / "Main.class").exists()


# AOTMode=on makes the JVM refuse a cache it would otherwise silently ignore; a
# warning, not a failure, because the image and the judge are updated separately.
@pytest.mark.kotlin
@pytest.mark.skipif(
    not kotlin_config.KOTLINC_AOT_CACHE.is_file(), reason="no Kotlin AOT cache"
)
def test_kotlin_reports_an_unusable_aot_cache(tmp_path: Path):
    (tmp_path / "Main.kt").write_text('fun main() { println("hi") }\n')
    command, _ = Kotlin(None).compilation(["Main.kt"])
    command.insert(1, "-XX:AOTMode=on")
    result = subprocess.run(
        command, cwd=tmp_path, capture_output=True, text=True, check=False
    )
    if result.returncode != 0:
        warnings.warn(
            f"{kotlin_config.KOTLINC_AOT_CACHE} does not fit the judge's JVM flags "
            "or classpath, so compiles run without it: "
            f"{(result.stdout + result.stderr)[:300]}",
            stacklevel=1,
        )


@pytest.mark.java
@pytest.mark.skipif(
    not java_config.JAVAC_AOT_CACHE.is_file(), reason="no Java AOT cache"
)
def test_java_reports_an_unusable_aot_cache(tmp_path: Path):
    (tmp_path / "Main.java").write_text(
        "public class Main { public static void main(String[] a) {} }\n"
    )
    command, _ = Java(None).compilation(["Main.java"])
    command.insert(1, "-J-XX:AOTMode=on")
    result = subprocess.run(
        command, cwd=tmp_path, capture_output=True, text=True, check=False
    )
    if result.returncode != 0:
        warnings.warn(
            f"{java_config.JAVAC_AOT_CACHE} does not fit the judge's JVM flags "
            "or classpath, so compiles run without it: "
            f"{(result.stdout + result.stderr)[:300]}",
            stacklevel=1,
        )
