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
import math
import os
import subprocess
import sys
from decimal import Decimal
from pathlib import Path

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
from tested.datatypes.advanced import AdvancedObjectTypes
from tested.dsl import parse_string
from tested.features import Construct, TypeSupport, fallback_type_support_map
from tested.judge.compilation import run_compilation
from tested.judge.execution import execute_file, filter_files
from tested.judge.utils import BaseExecutionResult, copy_from_paths_to_path
from tested.languages.conventionalize import conventionalize_namespace
from tested.oracles.value import _check_simple_type
from tested.serialisation import (
    BooleanType,
    ComparableFloat,
    NothingType,
    NumberType,
    ObjectKeyValuePair,
    ObjectType,
    SequenceType,
    SpecialNumbers,
    StringType,
    Value,
    WrappedAllTypes,
    parse_value,
    to_python_comparable,
)
from tested.testsuite import Suite
from tested.utils import get_args
from tests.language_markers import SERIALIZABLE_LANGUAGES
from tests.manual_utils import configuration

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
    NumberType(type=AdvancedNumericTypes.SINGLE_PRECISION, data=2.5),
    NumberType(type=AdvancedNumericTypes.SINGLE_PRECISION, data=2.5),
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
    StringType(type=AdvancedStringTypes.CHAR, data="h"),
    NothingType(type=AdvancedNothingTypes.UNDEFINED),
    ObjectType(
        type=AdvancedObjectTypes.DICTIONARY,
        data=[
            ObjectKeyValuePair(
                key=StringType(type=BasicStringTypes.TEXT, data="data"),
                value=NumberType(type=BasicNumericTypes.INTEGER, data=5),
            )
        ],
    ),
    ObjectType(
        type=AdvancedObjectTypes.OBJECT,
        data=[
            ObjectKeyValuePair(
                key=StringType(type=BasicStringTypes.TEXT, data="data"),
                value=NumberType(type=BasicNumericTypes.INTEGER, data=5),
            )
        ],
    ),
]


def run_encoder(bundle: Bundle, values: list[Value]) -> list[str]:
    # Copy dependencies.
    dependency_paths = bundle.language.path_to_dependencies()
    dependencies = bundle.language.initial_dependencies()
    dest = bundle.config.workdir / "encoder"
    dest.mkdir()
    copy_from_paths_to_path(dependency_paths, dependencies, dest)

    name = conventionalize_namespace(bundle.language, "encode")
    encoder_name = bundle.language.with_extension(name)
    encoder_destination = dest / encoder_name
    encode_code = bundle.language.generate_encoder(values)
    with open(encoder_destination, "w") as encoder_file:
        encoder_file.write(encode_code)

    # Compile if necessary.
    e, files = run_compilation(bundle, dest, [*dependencies, encoder_name], 10000)
    if isinstance(e, BaseExecutionResult):
        print(e.stdout)
        print(e.stderr)
        assert e.exit == 0

    files = filter_files(files, dest)
    files = bundle.language.filter_dependencies(files, name)
    executable = bundle.language.find_main_file(files, name)

    # Run the code.
    r = execute_file(bundle, executable.name, dest, None)
    print(r.stderr)
    return r.stdout.split(sep="␞")[:-1]


def assert_serialisation(bundle: Bundle, expected: Value):
    results = run_encoder(bundle, [expected])
    print(results)
    assert len(results) == 1
    actual = parse_value(results[0])
    assert actual.data == expected.data


@pytest.mark.parametrize("language", SERIALIZABLE_LANGUAGES)
def test_basic_types(language, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.language)

    # Create a list of basic types we want to test.
    types = [v for v in BASIC_VALUES if type_map[v.type] != TypeSupport.UNSUPPORTED]

    # Run the templates to encode the data.
    results = run_encoder(bundle, types)

    # Ensure each value was encoded properly.
    assert len(results) == len(types)

    for result, expected in zip(results, types):
        actual = parse_value(result)
        type_check, _ = _check_simple_type(bundle, expected, actual)
        assert type_check, f"type check failure {expected} != {actual}"
        py_expected = to_python_comparable(expected)
        py_actual = to_python_comparable(actual)
        assert py_expected == py_actual


@pytest.mark.parametrize("language", SERIALIZABLE_LANGUAGES)
def test_advanced_types(language, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.language)

    # Create a list of basic types we want to test.
    # We want to test all supported or reduced types.
    types: list[Value] = [
        v for v in ADVANCED_VALUES if type_map[v.type] != TypeSupport.UNSUPPORTED
    ]

    # Run the templates to encode the data.
    results = run_encoder(bundle, types)

    # Ensure each value was encoded properly.
    assert len(results) == len(types)

    for result, expected in zip(results, types):
        actual = parse_value(result)
        type_check, _ = _check_simple_type(bundle, expected, actual)
        assert type_check, f"type check failure {expected} != {actual}"
        py_expected = to_python_comparable(expected)
        py_actual = to_python_comparable(actual)
        assert py_expected == py_actual


@pytest.mark.parametrize("language", SERIALIZABLE_LANGUAGES)
def test_escape_double(language, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, StringType(type=BasicStringTypes.TEXT, data='"hallo"'))


@pytest.mark.parametrize("language", SERIALIZABLE_LANGUAGES)
def test_escape_single(language, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, StringType(type=AdvancedStringTypes.CHAR, data="'"))


@pytest.mark.parametrize("language", SERIALIZABLE_LANGUAGES)
def test_special_numbers(language, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.language)

    # Create a list of basic types we want to test.
    types: list[Value] = []
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
        type_check, _ = _check_simple_type(bundle, expected, actual)
        assert type_check, f"type check failure {expected} != {actual}"
        py_expected = to_python_comparable(expected)
        py_actual = to_python_comparable(actual)
        print(f"{actual} and {expected}")
        assert py_expected == py_actual


@pytest.mark.parametrize("language", SERIALIZABLE_LANGUAGES)
def test_floats_keep_full_precision(
    language, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.language)

    types: list[Value] = []
    for t, numbers in [
        (BasicNumericTypes.REAL, [1 / 3, 1e-7 / 3, 1e9 / 3, 0.1, 1e-20, -2.5]),
        (
            AdvancedNumericTypes.DOUBLE_PRECISION,
            [1 / 3, 1e-7 / 3, 1e9 / 3, 0.1, 1e-20, 1.0],
        ),
        (
            AdvancedNumericTypes.SINGLE_PRECISION,
            [0.5, -2.25, 0.125, 1.0, 3.0],
        ),
    ]:
        if type_map[t] == TypeSupport.SUPPORTED:
            types.extend(NumberType(type=t, data=n) for n in numbers)

    results = run_encoder(bundle, types)
    assert len(results) == len(types)

    for result, expected in zip(results, types):
        actual = parse_value(result)
        assert to_python_comparable(expected) == to_python_comparable(actual), result


@pytest.mark.parametrize(
    "type_",
    [
        BasicNumericTypes.REAL,
        AdvancedNumericTypes.SINGLE_PRECISION,
        AdvancedNumericTypes.DOUBLE_PRECISION,
    ],
)
def test_real_number_with_integral_data_becomes_float(type_):
    number = NumberType(type=type_, data=2)
    assert isinstance(number.data, float)
    assert number.data == 2.0


def test_integer_number_keeps_integral_data():
    number = NumberType(type=BasicNumericTypes.INTEGER, data=2)
    assert isinstance(number.data, int)


@pytest.mark.parametrize("language", SERIALIZABLE_LANGUAGES)
def test_valid_type_map(language: str, tmp_path: Path, pytestconfig: pytest.Config):
    # Get a type map.
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = fallback_type_support_map(bundle.language)

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


@pytest.mark.parametrize(
    "expression",
    [
        "[1, 'a']",
        "[[1], ['a']]",
        "[[1], 1]",
        "[[[1]], [['a']]]",
        "[[], [1], ['a']]",
        "[(1, 2), (1, 'a')]",
        "{(1,), ('a',)}",
        "[{'a': 1}, {'a': 'b'}]",
        "[{'a': 1}, {1: 1}]",
    ],
)
def test_heterogeneous_collections_are_detected(expression: str):
    features = parse_string(expression).get_used_features()
    assert Construct.HETEROGENEOUS_COLLECTIONS in features.constructs


@pytest.mark.parametrize(
    "expression",
    [
        "[1, 2]",
        "[[1], [2, 3]]",
        "[[1], []]",
        "[[], [1]]",
        "[[], []]",
        "[[[]], [[1]]]",
        "[[[1]], [[]], [[2], []]]",
        "[(1, 2), (3, 4)]",
        "{(1,), (2,)}",
        "[{'a': 1}, {'b': 2}]",
        "[{'a': []}, {'b': [1]}]",
        "[[1.5], [2.5]]",
    ],
)
def test_homogeneous_collections_are_not_flagged(expression: str):
    features = parse_string(expression).get_used_features()
    assert Construct.HETEROGENEOUS_COLLECTIONS not in features.constructs


def test_comparable_floats_are_equal_when_close():
    assert ComparableFloat(0.1 + 0.2) == ComparableFloat(0.3)
    assert ComparableFloat(math.nan) == ComparableFloat(math.nan)
    assert ComparableFloat(1.0) != ComparableFloat(2.0)
    assert ComparableFloat(math.nan) != ComparableFloat(1.0)


@pytest.mark.parametrize("other", [1.0, 1, "text", None, [1.0]])
def test_comparable_float_is_not_equal_to_other_types(other):
    assert ComparableFloat(1.0) != other
    assert other != ComparableFloat(1.0)


SEQUENCE = BasicSequenceTypes.SEQUENCE
MAP = BasicObjectTypes.MAP
INTEGER = BasicNumericTypes.INTEGER
ANY = BasicStringTypes.ANY
TEXT = BasicStringTypes.TEXT

COMBINED_TYPE_CASES: list[tuple[str, WrappedAllTypes]] = [
    ("[]", ANY),
    ("[[1], [2]]", (SEQUENCE, INTEGER)),
    ("[[[1]], [[1.0]]]", (SEQUENCE, (SEQUENCE, ANY))),
    ("[[[1]], [[[1]]]]", (SEQUENCE, (SEQUENCE, ANY))),
    ("[[1], 1]", ANY),
    ('[{"a": [1]}, {"a": [1.0]}]', (MAP, (TEXT, (SEQUENCE, ANY)))),
    ('{"a": [1], "b": {1: 2}}', (TEXT, ANY)),
    ("[{}, {1: 2}]", (MAP, (ANY, ANY))),
    ("{(1,): 1, (1.0,): 2}", ((AdvancedSequenceTypes.TUPLE, ANY), INTEGER)),
    ('[[{1: 1, "a": 1}], [[1, "a"]]]', (SEQUENCE, ANY)),
    ("[[[1]], [{1: 1}]]", (SEQUENCE, ANY)),
    ("[[[1]], [{1}]]", (SEQUENCE, ANY)),
    ('[[1, "a"], [{1: 1}]]', (SEQUENCE, ANY)),
]


def observed_combined_type(expression: str) -> WrappedAllTypes:
    value = parse_string(expression, True)
    if isinstance(value, ObjectType):
        return value.get_key_type(), value.get_value_type()
    assert isinstance(value, SequenceType)
    return value.get_content_type()


@pytest.mark.parametrize(("expression", "expected"), COMBINED_TYPE_CASES)
def test_combined_types(expression: str, expected: WrappedAllTypes):
    assert observed_combined_type(expression) == expected


@pytest.mark.parametrize("seed", ["0", "1", "2", "3", "42"])
def test_combined_types_do_not_depend_on_hash_seed(seed: str):
    script = (
        "from tests.test_serialisation import COMBINED_TYPE_CASES, observed_combined_type\n"
        "for expression, _ in COMBINED_TYPE_CASES:\n"
        "    print(repr(observed_combined_type(expression)))\n"
    )
    result = subprocess.run(
        [sys.executable, "-c", script],
        env={**os.environ, "PYTHONHASHSEED": seed},
        cwd=Path(__file__).parent.parent,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    expected = "".join(f"{case_type!r}\n" for _, case_type in COMBINED_TYPE_CASES)
    assert result.stdout == expected
