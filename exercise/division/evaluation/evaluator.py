import traceback
import specific_evaluation_utils as u


def evaluate(value):
    if isinstance(value, ZeroDivisionError):
        # If a zero division error, show the stacktrace.
        formatted = "".join(traceback.format_exception(type(value), value, value.__traceback__))
        u.evaluated(True, formatted, formatted)
    elif isinstance(value, Exception):
        # If another error, show the stacktrace as well.
        formatted = "".join(traceback.format_exception(type(value), value, value.__traceback__))
        u.evaluated(False, "ZeroDivisionError", formatted, [f"Verwachtte een ZeroDivisionError, maar kreeg een {type(value).__name__}."])
    else:
        # Else show the str of the value.
        actual = str(value) if value else ""
        u.evaluated(False, "ZeroDivisionError", actual, ["Verwachtte een ZeroDivisionError."])
