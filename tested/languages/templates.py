import logging
from pathlib import Path
from typing import List

from tested.configs import Bundle

_logger = logging.getLogger(__name__)


def path_to_dependencies(bundle: Bundle) -> List[Path]:
    """
    Construct the paths to the folder containing the additional dependencies
    needed for a programming language.

    :param bundle: The configuration bundle.

    :return: A list of template folders.
    """
    # TODO: fix Runhaskell
    judge_root = bundle.config.judge
    result = [
        judge_root
        / "tested"
        / "languages"
        / bundle.config.programming_language
        / "templates"
    ]
    assert result, "At least one template folder is required."
    return result
