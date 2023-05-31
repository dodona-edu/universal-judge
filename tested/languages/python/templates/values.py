"""Minimal RPC language in JSON to send data from the tests to the judge."""
import dataclasses
import decimal
import io
import json
import math
import traceback


def encode(value):
    diagnostic = None
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
        type_ = "bigint"
        data_ = value
    elif isinstance(value, float):
        type_ = "real"
        if math.isnan(value):
            data_ = "nan"
        elif math.isfinite(value):
            data_ = value
        elif value < 0:
            data_ = "-inf"
        else:
            data_ = "inf"
    elif isinstance(value, decimal.Decimal):
        type_ = "fixed_precision"
        if math.isnan(value):
            data_ = "nan"
        elif math.isfinite(value):
            data_ = str(value)
        elif value < 0:
            data_ = "-inf"
        else:
            data_ = "inf"
    elif isinstance(value, list):
        type_ = "list"
        data_ = [encode(x) for x in value]
    elif isinstance(value, tuple):
        type_ = "tuple"
        data_ = [encode(x) for x in value]
    elif isinstance(value, set):
        type_ = "set"
        data_ = [encode(x) for x in value]
    elif isinstance(value, dict):
        type_ = "map"
        data_ = [{"key": encode(k), "value": encode(v)} for k, v in value.items()]
    else:
        type_ = "unknown"
        data_ = str(value)
        diagnostic = str(type(value))

    return {"data": data_, "type": type_, "diagnostic": diagnostic}


def send_value(stream, value):
    """Send a value to the given stream."""
    json.dump(encode(value), stream)


def send_exception(stream, exception):
    if exception is None:
        return
    tracer = io.StringIO()
    traceback.print_tb(exception.__traceback__, file=tracer)
    data = {
        "message": str(exception),
        "stacktrace": f"{exception.__class__.__name__}: {exception}\n"
        f"{tracer.getvalue()}",
        "type": exception.__class__.__name__,
    }
    json.dump(data, stream)


class _EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if dataclasses.is_dataclass(o):
            return dataclasses.asdict(o)
        return super().default(o)


def send_evaluated(stream, r):
    json.dump(r, stream, cls=_EnhancedJSONEncoder)
