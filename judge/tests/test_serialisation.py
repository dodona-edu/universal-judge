"""
Test the serialisation format. While the normal exercise-based tests already use
the serialisation, they don't actually test all datatypes and such.

Testing advanced types is a work-in progress at this point, since we test in Python,
and Python does not have explicit support for e.g. int32, int64.
"""
from dataclasses import dataclass
from pathlib import Path
from typing import List

import pytest
import sys

from tested.configs import create_bundle, Bundle
from tested.datatypes import BasicNumericTypes, BasicStringTypes, BasicBooleanTypes, \
    BasicSequenceTypes, \
    BasicObjectTypes, BasicNothingTypes
from tested.judge.compilation import run_compilation
from tested.judge.execution import execute_file
from tested.judge.utils import copy_from_paths_to_path, BaseExecutionResult
from tested.languages.config import TypeSupport
from tested.languages.templates import find_and_write_template, path_to_templates
from tested.serialisation import NumberType, Value, parse_value, StringType, \
    BooleanType, SequenceType, ObjectType, \
    NothingType, as_basic_type, to_python_comparable
from tested.testplan import Plan
from tests.manual_utils import configuration

LANGUAGES = ["python", "java", "haskell", "c", "javascript"]


@dataclass
class _Statements:
    statements: List[Value]


def run_encoder(bundle: Bundle, dest: Path, values: List[Value]) -> List[str]:
    # Copy dependencies.
    dependency_paths = path_to_templates(bundle)
    dependencies = bundle.lang_config.initial_dependencies()
    copy_from_paths_to_path(dependency_paths, dependencies, dest)

    name = bundle.lang_config.conventionalize_namespace("encode")
    template = bundle.lang_config.template_name(name)
    encoder_name = bundle.lang_config.with_extension(name)
    encoder_destination = dest / encoder_name
    encoder = find_and_write_template(
        bundle, _Statements(values), encoder_destination, template
    )

    # Compile if necessary.
    e, _ = run_compilation(bundle, dest, [*dependencies, encoder], 10000)
    if isinstance(e, BaseExecutionResult):
        print(e.stdout)
        print(e.stderr)
        assert e.exit == 0

    # Run the code.
    r = execute_file(bundle, encoder, dest, None)
    print(r.stderr)
    return r.stdout.splitlines(keepends=False)


def assert_serialisation(bundle: Bundle, tmp_path: Path, expected: Value):
    results = run_encoder(bundle, tmp_path, [expected])
    print(results)
    assert len(results) == 1
    actual = parse_value(results[0])
    assert actual.data == expected.data


@pytest.mark.parametrize("language", ["javascript", "python", "runhaskell", "c", "java"])
def test_basic_types(language, tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Plan()
    bundle = create_bundle(conf, sys.stdout, plan)
    type_map = bundle.lang_config.type_support_map()

    # Create a list of basic types we want to test.
    types = []
    if type_map[BasicNumericTypes.INTEGER] != TypeSupport.UNSUPPORTED:
        types.append(NumberType(type=BasicNumericTypes.INTEGER, data=5))
    if type_map[BasicNumericTypes.RATIONAL] != TypeSupport.UNSUPPORTED:
        types.append(NumberType(type=BasicNumericTypes.RATIONAL, data=5.5))
    if type_map[BasicStringTypes.CHAR] != TypeSupport.UNSUPPORTED:
        types.append(StringType(type=BasicStringTypes.CHAR, data="c"))
    if type_map[BasicStringTypes.TEXT] != TypeSupport.UNSUPPORTED:
        types.append(StringType(type=BasicStringTypes.TEXT, data="hallo"))
    if type_map[BasicBooleanTypes.BOOLEAN] != TypeSupport.UNSUPPORTED:
        types.append(BooleanType(type=BasicBooleanTypes.BOOLEAN, data=True))
    if type_map[BasicSequenceTypes.SEQUENCE] != TypeSupport.UNSUPPORTED:
        types.append(SequenceType(type=BasicSequenceTypes.SEQUENCE, data=[
            NumberType(type=BasicNumericTypes.INTEGER, data=20)
        ]))
    if type_map[BasicSequenceTypes.SET] != TypeSupport.UNSUPPORTED:
        types.append(SequenceType(type=BasicSequenceTypes.SET, data=[
            NumberType(type=BasicNumericTypes.INTEGER, data=20)
        ]))
    if type_map[BasicObjectTypes.MAP] != TypeSupport.UNSUPPORTED:
        types.append(ObjectType(type=BasicObjectTypes.MAP, data={
            "data": NumberType(type=BasicNumericTypes.INTEGER, data=5)
        }))
    if type_map[BasicNothingTypes.NOTHING] != TypeSupport.UNSUPPORTED:
        types.append(NothingType())

    # Run the encode templates.
    results = run_encoder(bundle, tmp_path, types)

    assert len(results) == len(types)

    for result, expected in zip(results, types):
        actual = as_basic_type(parse_value(result))
        assert expected.type == actual.type
        py_expected = to_python_comparable(expected)
        py_actual = to_python_comparable(actual)
        assert py_expected == py_actual


def test_javascript_escape(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", "javascript", tmp_path)
    plan = Plan()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.TEXT, data='"hallo"'))


def test_python_escape(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", "python", tmp_path)
    plan = Plan()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.TEXT, data='"hallo"'))
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.TEXT, data="'hallo'"))


def test_java_escape(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", "java", tmp_path)
    plan = Plan()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.TEXT, data='"hallo"'))
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.CHAR, data="'"))


def test_haskell_escape(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", "runhaskell", tmp_path)
    plan = Plan()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.TEXT, data='"hallo"'))
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.CHAR, data="'"))


def test_c_escape(tmp_path: Path, pytestconfig):
    conf = configuration(pytestconfig, "", "c", tmp_path)
    plan = Plan()
    bundle = create_bundle(conf, sys.stdout, plan)
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.TEXT, data='"hallo"'))
    assert_serialisation(bundle, tmp_path, StringType(type=BasicStringTypes.CHAR, data="'"))
