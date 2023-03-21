# Tests for language-specific stuff, often in response to issues.
import sys
from pathlib import Path

from tested.configs import create_bundle
from tested.datatypes import BasicBooleanTypes, BasicNumericTypes, BasicStringTypes
from tested.languages.generator import convert_statement
from tested.serialisation import (
    BooleanType,
    FunctionCall,
    FunctionType,
    NumberType,
    StringType,
)
from tested.testsuite import Suite
from tests.manual_utils import configuration


def test_function_arguments_without_brackets(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", "haskell", tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)

    statement = FunctionCall(
        type=FunctionType.FUNCTION,
        name="test",
        namespace=None,
        arguments=[
            NumberType(type=BasicNumericTypes.REAL, data=5.5),
            StringType(type=BasicStringTypes.TEXT, data="hallo"),
            BooleanType(type=BasicBooleanTypes.BOOLEAN, data=True),
        ],
    )

    result = convert_statement(bundle, statement)
    assert (
        result
        == f'{bundle.lang_config.submission_name(plan)}.test 5.5 :: Double "hallo" True'
    )
