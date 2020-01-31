"""
Module with helper functions that are available to the language specific
evaluators. These help convert results to the correct output format.
This module is also available from the custom evaluator.
"""
import values
import sys

from typing import List, Optional


def evaluated(result: bool,
              readable_expected: Optional[str] = None,
              readable_actual: Optional[str] = None,
              messages: Optional[List[str]] = None):
    """
    Report the result of an evaluation to the judge. This method should only
    be called once, otherwise things will break.

    :param messages: Optional list of messages to be shown to the student.
    :param readable_actual: A string version of the actual value. Optional; if
                            not given, the judge will produce one on a best-
                            efforts basis.
    :param readable_expected: A string version of the expected value. Optional;
                              if not given, the judge will produce one on a
                              best-efforts basis.
    :param result: The result of the evaluation. True if accepted, false
                   otherwise.
    """
    if messages is None:
        messages = []

    values.send_evaluated(sys.stdout, result, readable_expected, readable_actual, messages)
