"""
Code related to working with templates.

TODO: these are mostly for older stuff. Once sorted, either remove them, or move
  them to translator.py
"""
from dataclasses import dataclass

from serialisation import Value


@dataclass
class CustomData:
    evaluator_code: str
    expected: Value
    actual: Value
