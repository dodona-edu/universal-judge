"""
Test the serialization format.

While the normal exercise-based tests already use serialization,
they don't actually test all datatypes and such.

To make testing easy, your language module needs to implement an "encode" template.
This template takes one value and must pass it to the "values" module.

Testing advanced types is a work-in progress at this point, since we test in Python,
and Python doesn't have explicit support for e.g. int32, int64.
"""
import itertools
import sys
from decimal import Decimal
from pathlib import Path
from typing import List

import pytest

from tested.configs import Bundle, create_bundle
from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    AdvancedStringTypes,
    AdvancedTypes,
    BasicBooleanTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BasicTypes,
    resolve_to_basic,
)
from tested.evaluators.value import check_data_type
from tested.features import TypeSupport, fallback_type_support_map
from tested.judge.compilation import run_compilation
from tested.judge.execution import execute_file, filter_files
from tested.judge.utils import BaseExecutionResult, copy_from_paths_to_path
from tested.languages.conventionalize import conventionalize_namespace
from tested.languages.templates import path_to_dependencies
from tested.serialisation import (
    BooleanType,
    NothingType,
    NumberType,
    ObjectKeyValuePair,
    ObjectType,
    SequenceType,
    SpecialNumbers,
    StringType,
    Value,
    parse_value,
    to_python_comparable,
)
from tested.testsuite import Suite
from tested.utils import get_args
from tests.manual_utils import configuration

LANGUAGES = [
    "python",
    "java",
    "c",
    "javascript",
    "kotlin",
    pytest.param("runhaskell", marks=pytest.mark.haskell),
    "bash",
    "csharp",
]


# A list of example values for basic types.
# Add types here you want to test for all languages.
BASIC_VALUES = [
    NumberType(type=BasicNumericTypes.INTEGER, data=5),
    NumberType(type=BasicNumericTypes.REAL, data=5.5),
    StringType(type=BasicStringTypes.TEXT, data="hallo"),
    BooleanType(type=BasicBooleanTypes.BOOLEAN, data=True),
    SequenceType(
        type=BasicSequenceTypes.SEQUENCE,
        data=[
            NumberType(type=BasicNumericTypes.INTEGER, data=20),
            NumberType(type=BasicNumericTypes.INTEGER, data=20),
        ],
    ),
    SequenceType(
        type=BasicSequenceTypes.SEQUENCE,
        data=[
            NumberType(type=BasicNumericTypes.INTEGER, data=20),
            NumberType(type=BasicNumericTypes.INTEGER, data=21),
        ],
    ),
    ObjectType(
        type=BasicObjectTypes.MAP,
        data=[
            ObjectKeyValuePair(
                key=StringType(type=BasicStringTypes.TEXT, data="data"),
                value=NumberType(type=BasicNumericTypes.INTEGER, data=5),
            )
        ],
    ),
    NothingType(),
]

# Map the advanced types to actual values that are already encoded in
# our serialization format.
ADVANCED_VALUES = [
    # Int 8
    NumberType(type=AdvancedNumericTypes.INT_8, data=5),
    NumberType(type=AdvancedNumericTypes.INT_8, data=-(2**7) + 1),
    NumberType(type=AdvancedNumericTypes.INT_8, data=(2**7) - 2),
    NumberType(type=AdvancedNumericTypes.U_INT_8, data=0),
    NumberType(type=AdvancedNumericTypes.U_INT_8, data=(2**8) - 1),
    # Int 16
    NumberType(type=AdvancedNumericTypes.INT_16, data=5),
    NumberType(type=AdvancedNumericTypes.INT_16, data=-(2**15) + 1),
    NumberType(type=AdvancedNumericTypes.INT_16, data=(2**15) - 2),
    NumberType(type=AdvancedNumericTypes.U_INT_16, data=0),
    NumberType(type=AdvancedNumericTypes.U_INT_16, data=(2**16) - 1),
    # Int 32
    NumberType(type=AdvancedNumericTypes.INT_32, data=5),
    NumberType(type=AdvancedNumericTypes.INT_32, data=-(2**31) + 1),
    NumberType(type=AdvancedNumericTypes.INT_32, data=(2**31) - 2),
    NumberType(type=AdvancedNumericTypes.U_INT_32, data=0),
    NumberType(type=AdvancedNumericTypes.U_INT_32, data=(2**32) - 1),
    # Int 64
    NumberType(type=AdvancedNumericTypes.INT_64, data=5),
    NumberType(type=AdvancedNumericTypes.INT_64, data=-(2**63) + 1),
    NumberType(type=AdvancedNumericTypes.INT_64, data=(2**63) - 2),
    NumberType(type=AdvancedNumericTypes.U_INT_64, data=0),
    NumberType(type=AdvancedNumericTypes.U_INT_64, data=(2**64) - 1),
    # Big int
    NumberType(type=AdvancedNumericTypes.BIG_INT, data=-(2**150)),
    NumberType(type=AdvancedNumericTypes.BIG_INT, data=2**150),
    # Floats
    NumberType(type=AdvancedNumericTypes.SINGLE_PRECISION, data=2.3),
    NumberType(type=AdvancedNumericTypes.SINGLE_PRECISION, data=2.3),
    NumberType(type=AdvancedNumericTypes.DOUBLE_PRECISION, data=0.3),
    NumberType(type=AdvancedNumericTypes.DOUBLE_PRECISION, data=-0.3),
    NumberType(
        type=AdvancedNumericTypes.FIXED_PRECISION, data=Decimal(1) / Decimal(32)
    ),
    # Sequences
    SequenceType(
        type=AdvancedSequenceTypes.ARRAY,
        data=[
            StringType(type=BasicStringTypes.TEXT, data="data"),
            StringType(type=BasicStringTypes.TEXT, data="data"),
        ],
    ),
    SequenceType(
        type=AdvancedSequenceTypes.LIST,
        data=[
            StringType(type=BasicStringTypes.TEXT, data="data"),
            StringType(type=BasicStringTypes.TEXT, data="data"),
        ],
    ),
    SequenceType(
        type=AdvancedSequenceTypes.TUPLE,
        data=[
            StringType(type=BasicStringTypes.TEXT, data="data"),
            StringType(type=BasicStringTypes.TEXT, data="data"),
        ],
    ),
    # Char
    StringType(type=AdvancedStringTypes.CHAR, data="h"),
    NothingType(type=AdvancedNothingTypes.UNDEFINED),
]


def run_encoder(bundle: Bundle, values: List[Value]) -> List[str]:
    # Copy dependencies.
    dependency_paths = path_to_dependencies(bundle)
    dependencies = bundle.lang_config.initial_dependencies()
    dest = bundle.config.workdir
    copy_from_paths_to_path(dependency_paths, dependencies, dest)

    name = conventionalize_namespace(bundle.lang_config, "encode")
    encoder_name = bundle.lang_config.with_extension(name)
    encoder_destination = dest / encoder_name
    encode_code = bundle.lang_config.generate_encoder(values)
    with open(encoder_destination, "w") as encoder_file:
        encoder_file.write(encode_code)

    # Compile if necessary.
    e, files = run_compilation(bundle, dest, [*dependencies, encoder_name], 10000)
    if isinstance(e, BaseExecutionResult):
        print(e.stdout)
        print(e.stderr)
        assert e.exit == 0

    files = filter_files(files, dest)
    files = bundle.lang_config.filter_dependencies(files, name)
    executable = bundle.lang_config.find_main_file(files, name, [])[0]

    # Run the code.
    r = execute_file(bundle, executable.name, dest, None)
    print(r.stderr)
    return r.stdout.splitlines(keepends=False)


def assert_serialisation(bundle: Bundle, expected: Value):
    results = run_encoder(bundle, [expected])
    print(results)
    assert len(results) == 1
    actual = parse_value(results[0])
    assert actual.data == expected.data


@pytest.mark.parametrize("language", LANGUAGES)
def test_basic_types(language, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.lang_config)

    # Create a list of basic types we want to test.
    types = [v for v in BASIC_VALUES if type_map[v.type] != TypeSupport.UNSUPPORTED]

    # Run the templates to encode the data.
    results = run_encoder(bundle, types)

    # Ensure each value was encoded properly.
    assert len(results) == len(types)

    for result, expected in zip(results, types):
        actual = parse_value(result)
        type_check, _ = check_data_type(bundle, expected, actual)
        assert type_check, f"type check failure {expected} != {actual}"
        py_expected = to_python_comparable(expected)
        py_actual = to_python_comparable(actual)
        assert py_expected == py_actual


@pytest.mark.parametrize("language", LANGUAGES)
def test_advanced_types(language, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.lang_config)

    # Create a list of basic types we want to test.
    # We want to test all supported or reduced types.
    types = [v for v in ADVANCED_VALUES if type_map[v.type] != TypeSupport.UNSUPPORTED]

    # Run the templates to encode the data.
    results = run_encoder(bundle, types)

    # Ensure each value was encoded properly.
    assert len(results) == len(types)

    for result, expected in zip(results, types):
        actual = parse_value(result)
        type_check, _ = check_data_type(bundle, expected, actual)
        assert type_check, f"type check failure {expected} != {actual}"
        py_expected = to_python_comparable(expected)
        py_actual = to_python_comparable(actual)
        assert py_expected == py_actual


@pytest.mark.parametrize("language", LANGUAGES)
def test_escape_double(language, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, StringType(type=BasicStringTypes.TEXT, data='"hallo"'))


@pytest.mark.parametrize("language", LANGUAGES)
def test_escape_single(language, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, StringType(type=AdvancedStringTypes.CHAR, data="'"))


@pytest.mark.parametrize("language", LANGUAGES)
def test_special_numbers(language, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.lang_config)

    # Create a list of basic types we want to test.
    types = []
    for t, n in itertools.product(
        [
            BasicNumericTypes.REAL,
            AdvancedNumericTypes.DOUBLE_PRECISION,
            AdvancedNumericTypes.SINGLE_PRECISION,
        ],
        [
            SpecialNumbers.NOT_A_NUMBER,
            SpecialNumbers.POS_INFINITY,
            SpecialNumbers.NEG_INFINITY,
        ],
    ):
        if type_map[t] == TypeSupport.SUPPORTED:
            types.append(NumberType(type=t, data=n))

    # Run the templates to encode the data.
    results = run_encoder(bundle, types)

    # Ensure each value was encoded properly.
    assert len(results) == len(types)

    for result, expected in zip(results, types):
        actual = parse_value(result)
        type_check, _ = check_data_type(bundle, expected, actual)
        assert type_check, f"type check failure {expected} != {actual}"
        py_expected = to_python_comparable(expected)
        py_actual = to_python_comparable(actual)
        print(f"{actual} and {expected}")
        assert py_expected == py_actual


@pytest.mark.parametrize("language", LANGUAGES)
def test_valid_type_map(language: str, tmp_path: Path, pytestconfig):
    # Get a type map.
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.lang_config)

    # Validate basic types.
    for basic_type in get_args(BasicTypes):
        value = type_map[basic_type]
        assert value in (TypeSupport.SUPPORTED, TypeSupport.UNSUPPORTED)

    # Validate advanced types.
    for advanced_type in get_args(AdvancedTypes):
        value = type_map[advanced_type]
        if value == TypeSupport.SUPPORTED or value == TypeSupport.REDUCED:
            basic_type = resolve_to_basic(advanced_type)
            basic_value = type_map[basic_type]
            assert basic_value == TypeSupport.SUPPORTED
