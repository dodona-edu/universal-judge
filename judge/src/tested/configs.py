"""
Module for handling and bundling various configuration options for TESTed.
"""
import dataclasses
import json
import logging
from collections import defaultdict
from dataclasses import field
from pathlib import Path
from typing import Optional, Dict, IO, TYPE_CHECKING, Any
from pydantic.dataclasses import dataclass

import tested.utils as utils
import tested.testplan as testplan

# Prevent circular imports
if TYPE_CHECKING:
    from tested.languages import Language


_logger = logging.getLogger(__name__)


@dataclass(frozen=True)
class Options:
    """
    TESTed-specific options. Putting these options in the exercise config allows to
    override them for each exercise, and not
    """
    parallel: bool = True
    """
    Indicate that the contexts should be executed in parallel. It is recommended to
    disable this for exercises that already are multithreaded. It may also be worth
    investigating if the exercise is computationally heady.
    """
    mode: testplan.ExecutionMode = testplan.ExecutionMode.PRECOMPILATION
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
    linter: Dict[str, bool] = field(default_factory=dict)
    """
    Controls running the linter for languages. Default is True. Of course, for
    languages without linter implementation, this does nothing.
    """
    show_unprocessed: bool = False
    """
    If testcases that were not run should be shown or not.
    """


@dataclass
class DodonaConfig:
    resources: Path
    source: Path
    time_limit: str
    memory_limit: str
    natural_language: str
    programming_language: str
    # noinspection SpellCheckingInspection
    workdir: Path
    judge: Path
    plan_name: str = "plan.json"  # Name of the testplan file.
    options: Options = Options()

    def config_for(self) -> dict:
        return self.options.language.get(self.programming_language, dict())

    def linter(self) -> bool:
        return self.options.linter.get(self.programming_language, False)


def read_config(config_in: IO) -> DodonaConfig:
    """
    Read the configuration from the given file. If the file is not stdin, it will be
    closed.
    """
    with utils.smart_close(config_in) as input_:
        config_json = input_.read()
    config_ = json.loads(config_json)

    # Replace the judge directory.
    parsed: DodonaConfig = DodonaConfig.__pydantic_model__.parse_obj(config_)
    parsed.judge = parsed.judge / 'judge' / 'src'

    return parsed


@dataclasses.dataclass(frozen=True)
class Bundle:
    """A bundle of arguments and configs for running everything."""
    config: DodonaConfig
    out: IO
    language_config: 'Language'
    secret: str
    plan: testplan.Plan


def _get_language(config: DodonaConfig) -> str:

    import tested.languages as langs

    bang = utils.consume_shebang(config.source)
    if bang and langs.language_exists(bang):
        _logger.debug(f"Found shebang language and it exists, using {bang} instead "
                      f"of config language {config.programming_language}.")
        return bang
    else:
        _logger.debug(f"No shebang found or it doesn't exist: {bang}. Using "
                      f"configuration language {config.programming_language}.")
        return config.programming_language


def create_bundle(config: DodonaConfig,
                  output: IO,
                  plan: testplan.Plan,
                  language: Optional[str] = None) -> Bundle:
    """
    Create a configuration bundle.

    :param config: The Dodona configuration.
    :param output: Where the output should go.
    :param plan: The testplan.
    :param language: Optional programming language. If None, the one from the Dodona
                     configuration will be used.

    :return: The configuration bundle.
    """
    import tested.languages as langs

    if language is None:
        language = _get_language(config)
    # noinspection PyDataclass
    adjusted_config = dataclasses.replace(config, programming_language=language)
    language_config = langs.get_language(language)
    return Bundle(
        config=adjusted_config,
        out=output,
        language_config=language_config,
        secret=utils.get_identifier(),
        plan=plan
    )
