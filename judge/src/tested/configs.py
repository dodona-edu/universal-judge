"""
Module for handling and bundling various configuration options for TESTed.
"""
import dataclasses
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Dict, IO, TYPE_CHECKING

import tested.utils as utils
import tested.testplan as testplan

# Prevent circular imports
if TYPE_CHECKING:
    from tested.languages import Language


@dataclass(frozen=True)
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
    linter: Optional[bool] = None
    plan_name: str = "plan.json"  # Name of the testplan file.
    options: Dict[str, str] = field(default_factory=dict)


def read_config(config_in: IO) -> DodonaConfig:
    """
    Read the configuration from the given file. If the file is not stdin, it will be
    closed.
    """
    with utils.smart_close(config_in) as input_:
        config_json = input_.read()
    config_ = json.loads(config_json)
    required = [x.name for x in dataclasses.fields(DodonaConfig)]
    needed_config = {x: config_[x] for x in required if x in config_}
    dodona = DodonaConfig(**needed_config)
    judge_dir = dodona.judge
    return dataclasses.replace(dodona, judge=judge_dir / 'judge' / 'src')


@dataclass(frozen=True)
class Bundle:
    """A bundle of arguments and configs for running everything."""
    config: DodonaConfig
    out: IO
    language_config: 'Language'
    secret: str
    plan: testplan.Plan


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
    from .languages import get_language

    if language is None:
        language = config.programming_language
    adjusted_config = dataclasses.replace(config, programming_language=language)
    language_config = get_language(language)
    return Bundle(
        config=adjusted_config,
        out=output,
        language_config=language_config,
        secret=utils.get_identifier(),
        plan=plan
    )
