"""
This package contains all things related to supporting programming languages. This
ranges from the templates for actually generating the code, to the actual code
needed to execute the templates.

In short, if it has to do with the templates or is programming language specific,
you will probably find it in this package.
"""
from languages.config import LanguageConfig
from languages.config.haskell import HaskellConfig
from languages.config.java import JavaConfig
from languages.config.python import PythonConfig


LANGUAGE_CONFIGS = {
    'python':  PythonConfig,
    'java':    JavaConfig,
    'haskell': HaskellConfig
}


def get_language_config(language: str) -> LanguageConfig:
    """
    Get the configuration for a programming language.
    """
    return LANGUAGE_CONFIGS[language]()
