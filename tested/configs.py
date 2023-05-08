"""
Module for handling and bundling various configuration options for TESTed.
"""
import dataclasses
import json
import logging
from dataclasses import field
from pathlib import Path
from typing import IO, TYPE_CHECKING, Any, Dict, Optional, Tuple

from pydantic import BaseModel, root_validator
from pydantic.dataclasses import dataclass

from tested.testsuite import ExecutionMode, Suite
from tested.utils import consume_shebang, get_identifier, smart_close

# Prevent circular imports
if TYPE_CHECKING:
    from tested.languages import Language

_logger = logging.getLogger(__name__)


@dataclass(frozen=True)
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
    language: Dict[str, Dict[str, Any]] = field(default_factory=dict)
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
    If the custom Python evaluator should be optimized or not.
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


class DodonaConfig(BaseModel):
    resources: Path
    source: Path
    time_limit: int
    memory_limit: int
    natural_language: str
    programming_language: str
    # noinspection SpellCheckingInspection
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

    def config_for(self) -> Dict[str, Any]:
        return self.options.language.get(self.programming_language, dict())

    def linter(self) -> bool:
        local_config = self.config_for().get("linter", None)
        if local_config is None:
            return self.options.linter
        return local_config

    # noinspection PyMethodParameters
    @root_validator(pre=True)
    def backward_compatibility_for_test_suite(cls, values):
        compatible_names = ["testplan", "plan_name"]
        for old_name in compatible_names:
            if old_name in values:
                values["test_suite"] = values[old_name]
                del values[old_name]
        return values


def read_config(config_in: IO) -> DodonaConfig:
    """
    Read the configuration from the given file. If the file is not stdin, it will be
    closed.
    """
    with smart_close(config_in) as input_:
        config_json = input_.read()
    config_ = json.loads(config_json)

    # Replace the judge directory.
    parsed: DodonaConfig = DodonaConfig.parse_obj(config_)
    # noinspection PyDataclass
    parsed = parsed.copy(update={"judge": parsed.judge})

    return parsed


@dataclasses.dataclass
class GlobalConfig:
    dodona: DodonaConfig
    testcase_separator_secret: str
    context_separator_secret: str
    suite: "Suite"


@dataclasses.dataclass
class Bundle:
    """A bundle of arguments and configs for running everything."""

    lang_config: "Language"
    global_config: GlobalConfig
    out: IO

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

    @property
    def options(self) -> Options:
        return self.global_config.dodona.options


def _get_language(config: DodonaConfig) -> Tuple[str, int]:
    import tested.languages as langs

    bang = consume_shebang(config.source)
    if bang and langs.language_exists(bang):
        _logger.debug(
            f"Found shebang language and it exists, using {bang} instead "
            f"of config language {config.programming_language}."
        )
        return bang, 1
    else:
        _logger.debug(
            f"No shebang found or it doesn't exist: {bang}. Using "
            f"configuration language {config.programming_language}."
        )
        return config.programming_language, 0


def create_bundle(
    config: DodonaConfig,
    output: IO,
    suite: Suite,
    language: Optional[str] = None,
) -> Bundle:
    """
    Create a configuration bundle.

    :param config: The Dodona configuration.
    :param output: Where the output should go.
    :param suite: The test suite.
    :param language: Optional programming language. If None, the one from the Dodona
                     configuration will be used.

    :return: The configuration bundle.
    """
    import tested.languages as langs

    if language is None:
        language, offset = _get_language(config)
        config.source_offset = offset
    # noinspection PyDataclass
    adjusted_config = config.copy(update={"programming_language": language})
    global_config = GlobalConfig(
        dodona=adjusted_config,
        testcase_separator_secret=get_identifier(),
        context_separator_secret=get_identifier(),
        suite=suite,
    )
    lang_config = langs.get_language(global_config, language)
    return Bundle(lang_config=lang_config, global_config=global_config, out=output)
