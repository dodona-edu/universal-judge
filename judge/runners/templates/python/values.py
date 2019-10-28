"""Minimal RPC language in JSON to send data from the tests to the judge."""
import json


def send(stream, value):
    """Send a value to the given stream."""
    if value is None:
        return
    if isinstance(value, str):
        type_ = "text"
    elif isinstance(value, int):
        type_ = "integer"
    elif isinstance(value, float):
        type_ = "rational"
    elif isinstance(value, bool):
        type_ = "boolean"
    else:
        type_ = "unknown"
    json.dump({
        "data": value,
        "type": type_
    }, stream)
