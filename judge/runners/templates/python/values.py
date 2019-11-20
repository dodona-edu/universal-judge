"""Minimal RPC language in JSON to send data from the tests to the judge."""
import json


def __encode(value):
    if value is None:
        type_ = "nothing"
        data_ = value
    elif isinstance(value, str):
        type_ = "text"
        data_ = value
    elif isinstance(value, int):
        type_ = "integer"
        data_ = value
    elif isinstance(value, float):
        type_ = "rational"
        data_ = value
    elif isinstance(value, bool):
        type_ = "boolean"
        data_ = value
    elif isinstance(value, list) or isinstance(value, tuple):
        type_ = "list"
        data_ = [__encode(x) for x in value]
    elif isinstance(value, set):
        type_ = "set"
        data_ = [__encode(x) for x in value]
    elif isinstance(value, dict):
        type_ = "object"
        data_ = {str(k): __encode(v) for k, v in value.items()}
    else:
        type_ = "unknown"
        data_ = str(value)

    return type_, data_


def send_value(stream, value):
    """Send a value to the given stream."""
    if (encoded := __encode(value)) is not None:
        json.dump({
            "data": encoded[1],
            "type": encoded[0]
        }, stream)


# noinspection PyDefaultArgument
def send_evaluated(stream, result, expected, actual, messages):
    json.dump({
        "result": result,
        "readable_expected": expected,
        "readable_actual": actual,
        "messages": messages
    }, stream)
