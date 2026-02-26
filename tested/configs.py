"""
Module for handling and bundling various configuration options for TESTed.
"""

import logging
from pathlib import Path
from typing import IO, TYPE_CHECKING, Any, Optional

from attrs import define, evolve, field

from tested.dodona import ExtendedMessage
from tested.parsing import fallback_field, get_converter
from tested.testsuite import ExecutionMode, Suite, SupportedLanguage
from tested.utils import get_identifier, smart_close

# Prevent circular imports
if TYPE_CHECKING:
    from tested.languages import Language

_logger = logging.getLogger(__name__)


@define(frozen=True)
class Options:
    """
    TESTed-specific options. Putting these options in the exercise config allows
    overriding them for each exercise.
    """

    parallel: bool = False
    """
    Indicate that the contexts should be executed in parallel. It is recommended to
    disable this for exercises that already are multithreaded. It may also be worth
    investigating if the exercise is computationally heady.
    """
    mode: ExecutionMode = ExecutionMode.PRECOMPILATION
    """
    The default mode for the judge.
    """
    allow_fallback: bool = True
    """
    Indicate if the judge should attempt individual mode if the precompilation mode
    fails. If nothing is given, the language-dependent default is used. If a boolean
    is given, this value is used, regardless of the language default.
    """
    language: dict[str, dict[str, Any]] = field(factory=dict)
    """
    Language-specific options for the judge. These depend on the language
    implementation; the judge itself does nothing with it.
    """
    linter: bool = True
    """
    Controls running the linter for languages. Default is True. Of course, for
    languages without linter implementation, this does nothing.
    """
    optimized: bool = True
    """
    If the Python oracles should be optimized or not.
    """
    compiler_optimizations: bool = False
    """
    If compiler optimizations should be enabled for languages that support them,
    for example, C or Haskell.
    This is disabled by default, as the additional time required in the compilation
    step is often much larger than the gains in execution time for short exercises.
    Longer exercises, or exercises where the solution might depend on optimization
    may need this option.
    """


@fallback_field(get_converter(), {"testplan": "test_suite", "plan_name": "test_suite"})
@define
class DodonaConfig:
    resources: Path
    source: Path
    time_limit: int
    memory_limit: int
    natural_language: str
    programming_language: SupportedLanguage
    workdir: Path
    judge: Path
    test_suite: str = (
        "plan.json"  # We cannot change this default, as it breaks exercises.
    )
    options: Options = Options()
    output_limit: int = 10240 * 1024  # Default value for backward compatibility.
    timing_statistics: bool = False

    # Sometimes, we need to offset the source code.
    source_offset: int = 0

    def config_for(self) -> dict[str, Any]:
        return self.options.language.get(self.programming_language, dict())

    def linter(self) -> bool:
        local_config = self.config_for().get("linter", None)
        if local_config is None:
            return self.options.linter
        return local_config


def read_config(config_in: IO) -> DodonaConfig:
    """
    Read the configuration from the given file. If the file is not stdin, it will be
    closed.
    """
    with smart_close(config_in) as input_:
        config_json = input_.read()

    parsed = get_converter().loads(config_json, DodonaConfig)
    return parsed


@define
class GlobalConfig:
    dodona: DodonaConfig
    testcase_separator_secret: str
    context_separator_secret: str
    suite: "Suite"

    @property
    def options(self) -> Options:
        return self.dodona.options


@define
class Bundle:
    """A bundle of arguments and configs for running everything."""

    language: "Language"
    global_config: GlobalConfig
    out: IO
    messages: set[ExtendedMessage] = set()

    @property
    def config(self) -> DodonaConfig:
        return self.global_config.dodona

    @property
    def suite(self) -> "Suite":
        return self.global_config.suite

    @property
    def testcase_separator_secret(self) -> str:
        return self.global_config.testcase_separator_secret

    @property
    def context_separator_secret(self) -> str:
        return self.global_config.context_separator_secret


def _consume_shebang(submission: Path) -> Optional["SupportedLanguage"]:
    """
    Find the shebang in the submission, and if it is present, consume it.

    :param submission: The path to the file containing the code.

    :return: The programming language if found.
    """
    from tested.testsuite import SupportedLanguage

    language = None
    try:
        with open(submission, "r+") as file:
            lines = file.readlines()
            file.seek(0)

            # Steps to find
            has_potential = True
            for line in lines:
                stripped = line.strip()
                if has_potential and stripped.startswith("#!tested"):
                    try:
                        _, language = stripped.split(" ")
                    except ValueError:
                        _logger.error(f"Invalid shebang on line {stripped}")
                else:
                    file.write(line)
                if has_potential and stripped:
                    has_potential = False
            file.truncate()
    except FileNotFoundError:
        # Nothing we can do if there is no submission.
        pass

    return SupportedLanguage(language) if language else None


def _get_language(config: DodonaConfig) -> tuple[SupportedLanguage, int]:
    import tested.languages as langs

    bang = _consume_shebang(config.source)
    if bang and langs.language_exists(bang):
        _logger.debug(
            "Found shebang language and it exists, using %s instead of config language %s.",
            bang,
            config.programming_language,
        )
        return bang, 1
    else:
        _logger.debug(
            "No shebang found or it doesn't exist: %s. Using configuration language %s.",
            bang,
            config.programming_language,
        )
        return config.programming_language, 0


def create_bundle(
    config: DodonaConfig,
    output: IO,
    suite: Suite,
    language: str | None = None,
    messages: set[ExtendedMessage] | None = None,
) -> Bundle:
    """
    Create a configuration bundle.

    :param config: The Dodona configuration.
    :param output: Where the output should go.
    :param suite: The test suite.
    :param language: Optional programming language. If None, the one from the Dodona
                     configuration will be used.
    :param messages: Messages generated out of the translate parser.
    :return: The configuration bundle.
    """
    import tested.languages as langs

    if language is None:
        language, offset = _get_language(config)
        config.source_offset = offset
    adjusted_config = evolve(config, programming_language=language)
    global_config = GlobalConfig(
        dodona=adjusted_config,
        testcase_separator_secret=get_identifier(),
        context_separator_secret=get_identifier(),
        suite=suite,
    )
    lang_config = langs.get_language(global_config, language)

    if messages is None:
        messages = set()

    return Bundle(
        language=lang_config, global_config=global_config, out=output, messages=messages
    )
