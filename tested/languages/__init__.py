"""
This package contains all things related to supporting programming languages. This
ranges from the templates for actually generating the code, to the actual code
needed to execute the templates.

In short, if it has to do with the templates or is programming language specific,
you will probably find it in this package.
"""

from typing import TYPE_CHECKING, Optional

from tested.languages.bash.config import Bash
from tested.languages.c.config import C
from tested.languages.csharp.config import CSharp
from tested.languages.haskell.config import Haskell
from tested.languages.java.config import Java
from tested.languages.javascript.config import JavaScript
from tested.languages.kotlin.config import Kotlin
from tested.languages.language import Language
from tested.languages.nextflow.config import Nextflow
from tested.languages.python.config import Python
from tested.languages.runhaskell.config import RunHaskell

if TYPE_CHECKING:
    from tested.configs import GlobalConfig


LANGUAGES = {
    "bash": Bash,
    "c": C,
    "haskell": Haskell,
    "java": Java,
    "javascript": JavaScript,
    "kotlin": Kotlin,
    "nextflow": Nextflow,
    "python": Python,
    "runhaskell": RunHaskell,
    "csharp": CSharp,
}


def get_language(global_config: Optional["GlobalConfig"], language: str) -> Language:
    """
    Get the configuration for a programming language.
    """
    return LANGUAGES[language](global_config)


def language_exists(language: str) -> bool:
    return language in LANGUAGES
