from dataclasses import is_dataclass, asdict
from decimal import Decimal
from json import JSONEncoder


# Idea: Stackoverflow:
# https://stackoverflow.com/questions/51286748/make-the-python-json-encoder
# -support-pythons-new-dataclasses#answer-51286749
class DataclassJSONEncoder(JSONEncoder):
    def default(self, o):
        if is_dataclass(o):
            return asdict(o)
        if isinstance(o, Decimal):
            if o == o.to_integral_value():
                return int(o)
            else:
                return float(o)

        return super().default(o)
