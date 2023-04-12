import logging
from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages import get_language

_logger = logging.getLogger(__name__)


def _language_inheritance_tree(bundle: Bundle) -> List[str]:
    current = bundle.lang_config
    result = [bundle.config.programming_language]
    while lang := current.inherits_from():
        result.append(lang)
        current = get_language(lang)
    return result


def path_to_dependencies(bundle: Bundle) -> List[Path]:
    """
    Construct the paths to the folder containing the additional dependencies
    needed for a programming language.

    :param bundle: The configuration bundle.

    :return: A list of template folders.
    """
    judge_root = bundle.config.judge
    result = []
    for language in _language_inheritance_tree(bundle):
        result.append(judge_root / "tested" / "languages" / language / "templates")
    assert result, "At least one template folder is required."
    return result
