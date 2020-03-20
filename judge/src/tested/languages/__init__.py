"""
This package contains all things related to supporting programming languages. This
ranges from the templates for actually generating the code, to the actual code
needed to execute the templates.

In short, if it has to do with the templates or is programming language specific,
you will probably find it in this package.
"""
from .config import Language
from .configs.haskell import HaskellConfig
from .configs.java import JavaConfig
from .configs.python import Python


LANGUAGES = {
    'python':  Python,
    'java':    JavaConfig,
    'haskell': HaskellConfig
}


def get_language(language: str) -> Language:
    """
    Get the configuration for a programming language.
    """
    return LANGUAGES[language]()
