"""
Generates code for the testplan execution.

The API entry point is the :class:`Runner` class. The main responsibility of this
class is translate and execute a testplan.

Broadly speaking, the responsibilities can be divided into a few stages:
1. Generate the code for the execution of the testplan.
2. Execute the generated code.

"""
import logging
from typing import Set

from runners.config import LanguageConfig
from runners.languages.haskell import HaskellConfig
from runners.languages.java import JavaConfig
from runners.languages.python import PythonConfig
from testplan import Plan

logger = logging.getLogger(__name__)


CONFIGS = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig
}


def get_languages() -> Set[str]:
    """
    :return: Languages supported by the judge.
    """
    return set(CONFIGS.keys())


def get_supporting_languages(plan: Plan) -> Set[str]:
    """
    :param plan: The testplan.
    :return: The languages that have the required features to execute the testplan.
    """
    required = plan.get_used_features()
    supported_languages = set()
    for language, config in CONFIGS.items():
        supported_features = config().supported_features()
        if supported_features & required != 0:
            supported_languages.add(language)
    return supported_languages


def get_language_config(language: str) -> LanguageConfig:
    return CONFIGS[language]()
