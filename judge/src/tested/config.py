"""
Module for handling and bundling various configuration options for TESTed.
"""
import dataclasses
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Dict, IO

from .utils import smart_close



@dataclass(frozen=True)
class Config:
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


def read_config(config_in: IO) -> Config:
    """
    Read the configuration from the given file. If the file is not stdin, it will be
    closed.
    """
    with smart_close(config_in) as input_:
        config_json = input_.read()
    config_ = json.loads(config_json)
    required = [x.name for x in dataclasses.fields(Config)]
    needed_config = {x: config_[x] for x in required if x in config_}
    return Config(**needed_config)


@dataclass(frozen=True)
class Bundle:
    """A bundle of arguments and config for running everything."""
    config: Config
    out: IO
    language_config: LanguageConfig
    secret: str
    plan: Plan


def create_bundle(config: Config,
                  output: IO,
                  plan: Plan,
                  language: Optional[str] = None) -> Bundle:
    if language is None:
        language = config.programming_language
    adjusted_config = dataclasses.replace(config, programming_language=language)
    language_config = get_language_config(language)
    return Bundle(
        config=adjusted_config,
        out=output,
        language_config=language_config,
        secret=_get_identifier(),
        plan=plan
    )
