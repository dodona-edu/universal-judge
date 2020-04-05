import values

from typing import List, Optional


out = None


def evaluated(result: bool,
              readable_expected: Optional[str] = None,
              readable_actual: Optional[str] = None,
              messages: Optional[List[str]] = None):
    """
    Report the result of an evaluation to the judge. This method should only
    be called once per evaluation. Calling this multiple times will result in
    undefined behaviour.

    :param result: The result of the evaluation (True or False).

    :param readable_expected: A string version of the channel value. Optional;
                              if not given, the judge will produce one on a
                              best-efforts basis.
    :param readable_actual: A string version of the actual value. Optional; if
                            not given, the judge will produce one on a best-
                            efforts basis.
    :param messages: Optional list of messages to be shown to the student.
    """
    if messages is None:
        messages = []

    values.send_evaluated(out,
                          result, readable_expected, readable_actual, messages)
