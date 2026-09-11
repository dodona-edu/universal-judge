from typing import Any

from attrs import define

from tested.datatypes import AllTypes


@define
class TestedType:
    value: Any
    type: str | AllTypes


class ExpressionString(str):
    pass


class ReturnOracle(dict):
    pass


class ContentPathString(str):
    pass


type OptionDict = dict[str, int | bool]

type YamlDict = dict[str, "YamlObject"]

type YamlObject = (
    YamlDict
    | list
    | bool
    | float
    | int
    | str
    | None
    | ExpressionString
    | ReturnOracle
    | ContentPathString
)


class DslValidationError(ValueError):
    pass


class InvalidYamlError(ValueError):
    pass
