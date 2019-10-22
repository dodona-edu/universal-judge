"""Minimal RPC language in JSON to send data from the tests to the judge."""
import json


def send(stream, value):
    """Send a value to the given stream."""
    if value is None:
        pass
    if isinstance(value, str):
        json.dump({
            "data": value,
            "type": "text"
        }, stream)
    elif isinstance(value, int):
        json.dump({
            "data": value,
            "type": "integer"
        }, stream)
    elif isinstance(value, float):
        json.dump({
            "data": value,
            "type": "rational"
        }, stream)
    else:
        json.dump({
            "data": value,
            "type": "unknown"
        }, stream)
