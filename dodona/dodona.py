import sys

from dodona.partial_output import _Update
from .full_output import Feedback

import dataclasses, json


def _clean_dictionary(d):
    if not isinstance(d, dict):
        return d
    return {k: _clean_dictionary(v) for k, v in d.items() if v is not None}


def _class_and_instance_attributes(object_) -> dict:
    # Get instance attributes
    d = dataclasses.asdict(object_)
    # Add class attributes
    for key, value in vars(object_.__class__).items():
        if key[:2] != '__' and not callable(value):
            d[key] = getattr(object_, key)
    return d


class _EnhancedJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if dataclasses.is_dataclass(o):
            return _clean_dictionary(_class_and_instance_attributes(o))
        return super().default(o)


def report_feedback(feedback: Feedback):
    """
    Send feedback to stdout.
    """
    json.dump(feedback, sys.stdout, cls=_EnhancedJSONEncoder)
    # print()


def report_update(update: _Update):
    """
    Send an update to stdout.
    """
    json.dump(update, sys.stdout, cls=_EnhancedJSONEncoder)
    # print()
