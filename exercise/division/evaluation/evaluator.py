import traceback
import specific_evaluation_utils as u


def evaluate(value):
    if isinstance(value, ZeroDivisionError):
        formatted = "".join(traceback.format_exception(type(value), value, value.__traceback__))
        u.evaluated(True, formatted, formatted)
    elif isinstance(value, Exception):
        formatted = "".join(traceback.format_exception(type(value), value, value.__traceback__))
        u.evaluated(False, "ZeroDivisionError", formatted, ["Verwachtte een ZeroDivisionError."])
    else:
        actual = str(value) if value else ""
        u.evaluated(False, "ZeroDivisionError", actual, ["Verwachtte een ZeroDivisionError."])
