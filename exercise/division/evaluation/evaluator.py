import traceback
import specific_evaluation_utils as u


def evaluate(value):
    if isinstance(value, ZeroDivisionError):
        formatted = "".join(traceback.format_exception(type(value), value, value.__traceback__))
        u.evaluated(True, formatted, formatted)
    else:
        u.evaluated(False, "ZeroDivisionError", str(value), ["Verwachtte een ZeroDivisionError."])
