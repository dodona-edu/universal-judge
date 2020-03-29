"""
Module for handling and bundling various configuration options for TESTed.
"""
import dataclasses
import json
import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Dict, IO, TYPE_CHECKING

import tested.utils as utils
import tested.testplan as testplan

# Prevent circular imports
if TYPE_CHECKING:
    from tested.languages import Language


_logger = logging.getLogger(__name__)


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

    # The configuration we receive from Dodona can contain additional fields that
    # we don't use. To prevent errors, only take the fields we use. Additionally,
    # convert some fields to Paths, since we like typed attributes in TESTed.
    required = dict()
    for field_ in dataclasses.fields(DodonaConfig):
        if field_.name not in config_:
            continue

        # Special support for Paths
        if field_.type == Path:
            required[field_.name] = Path(config_[field_.name])
        else:
            # Just take in the value.
            required[field_.name] = config_[field_.name]

    dodona = DodonaConfig(**required)
    judge_dir = dodona.judge
    return dataclasses.replace(dodona, judge=judge_dir / 'judge' / 'src')


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
    adjusted_config = dataclasses.replace(config, programming_language=language)
    language_config = langs.get_language(language)
    return Bundle(
        config=adjusted_config,
        out=output,
        language_config=language_config,
        secret=utils.get_identifier(),
        plan=plan
    )



