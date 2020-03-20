"""Minimal RPC language in JSON to send data from the tests to the judge."""
import io
import json
import traceback


def encode(value):
    if value is None:
        type_ = "nothing"
        data_ = value
    elif isinstance(value, str):
        type_ = "text"
        data_ = value
    elif isinstance(value, bool):
        type_ = "boolean"
        data_ = value
    elif isinstance(value, int):
        type_ = "integer"
        data_ = value
    elif isinstance(value, float):
        type_ = "rational"
        data_ = value
    elif isinstance(value, list) or isinstance(value, tuple):
        type_ = "sequence"
        data_ = [encode(x) for x in value]
    elif isinstance(value, set):
        type_ = "set"
        data_ = [encode(x) for x in value]
    elif isinstance(value, dict):
        type_ = "map"
        data_ = {str(k): encode(v) for k, v in value.items()}
    else:
        type_ = "unknown"
        data_ = str(value)

    return {
        "data": data_,
        "type": type_
    }


def send_value(stream, value):
    """Send a value to the given stream."""
    json.dump(encode(value), stream)


def send_exception(stream, exception):
    tracer = io.StringIO()
    traceback.print_tb(exception.__traceback__, file=tracer)
    data = {
        "message": str(exception),
        "stacktrace": tracer.getvalue()
    }
    json.dump(data, stream)


# noinspection PyDefaultArgument
def send_evaluated(stream, result, expected, actual, messages):
    json.dump({
        "result": result,
        "readable_expected": expected,
        "readable_actual": actual,
        "messages": messages
    }, stream)
