"""
Tests for specific aspects of certain language implementations.
"""

import itertools
import shutil
import sys
from pathlib import Path

import pytest

from tested.configs import create_bundle
from tested.datatypes import BasicBooleanTypes, BasicNumericTypes, BasicStringTypes
from tested.dsl import parse_string
from tested.languages import LANGUAGES
from tested.languages.conventionalize import submission_name
from tested.languages.generation import generate_statement
from tested.serialisation import (
    BooleanType,
    FunctionCall,
    FunctionType,
    NumberType,
    StringType,
)
from tested.testsuite import Suite
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_cpp_function_assignment(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = "test: string = foo()"
    cpp = LANGUAGES["cpp"](pytestconfig)
    result = cpp.generate_statement(parse_string(statement_string))

    assert result == "std::string test = foo()"


def test_cpp_complex_function_assignment(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = 'test = { "a": foo() }'
    cpp = LANGUAGES["cpp"](pytestconfig)
    result = cpp.generate_statement(parse_string(statement_string))

    assert result == 'std::map<std::string, std::any> test = {{"a", foo()}}'


def test_cpp_types_get_cast_within_functions(
    tmp_path: Path, pytestconfig: pytest.Config
):
    statement_string = 'test: string = foo(1, "2", [3, 4], (5, 6), {7: 8})'
    cpp = LANGUAGES["cpp"](pytestconfig)
    result = cpp.generate_statement(parse_string(statement_string))

    int_p = "std::intmax_t(1)"
    str_p = 'std::string("2")'
    vec_p = "std::vector<std::intmax_t>({3, 4})"
    tuple_p = "std::tuple<std::intmax_t, std::intmax_t>({5, 6})"
    map_p = "std::map<std::intmax_t, std::intmax_t>({{7, 8}})"
    assert (
        result
        == f"std::string test = foo({int_p}, {str_p}, {vec_p}, {tuple_p}, {map_p})"
    )


def test_cpp_complex_type_assignment(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = "test = {'1': [2 , (3, {4, 5})]}"
    cpp = LANGUAGES["cpp"](pytestconfig)
    result = cpp.generate_statement(parse_string(statement_string))

    variant_types = [
        "std::intmax_t",
        "std::tuple<std::intmax_t, std::set<std::intmax_t>>",
    ]
    permutations = list(itertools.permutations(variant_types))
    valid_types = [
        f"std::map<std::string, std::vector<std::variant<{", ".join(perm)}>>>"
        for perm in permutations
    ]
    valid_value = '{{"1", {2, {3, {4, 5}}}}}'
    valid_results = [f"{tp1} test = {valid_value}" for tp1 in valid_types]

    assert result in valid_results


def test_cpp_static_functions(tmp_path: Path, pytestconfig: pytest.Config):
    cpp = LANGUAGES["cpp"](pytestconfig)
    object_function = "foo.bar()"
    result = cpp.generate_statement(parse_string(object_function))

    assert result == "foo.bar()"

    class_function = "Foo.bar()"
    result = cpp.generate_statement(parse_string(class_function))

    assert result == "Foo::bar()"

    unknown_function = "foo().bar()"
    result = cpp.generate_statement(parse_string(unknown_function))

    assert result == "(foo()).bar()"


def test_cpp_unknown_return_type_without_streamable_is_null(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "cpp",
        tmp_path,
        "one.tson",
        "unknown-return-type-non-streamable",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    received_data = updates.find_next("close-test")["generated"]
    assert received_data == "None"


def test_typescript_array_typing(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = "test = ['test', True, 10, 10.1, None, {'wow': 10}]"
    ts = LANGUAGES["typescript"](pytestconfig)
    result = ts.generate_statement(parse_string(statement_string))
    types = ["string", "boolean", "number", "object", "null"]
    permutations = list(itertools.permutations(types))
    valid_results = [
        f'let test : Array<{"|".join(perm)}> = ["test", true, 10, 10.1, null, new Map([["wow", 10]])]'
        for perm in permutations
    ]

    assert result in valid_results


def test_typescript_set_typing(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = "test = {'test', True, 10, 10.1, None, {'wow': 10}}"
    ts = LANGUAGES["typescript"](pytestconfig)
    result = ts.generate_statement(parse_string(statement_string))
    types = ["string", "boolean", "number", "object", "null"]
    permutations = list(itertools.permutations(types))
    valid_results = [
        f'let test : Set<{"|".join(perm)}> = new Set(["test", true, 10, 10.1, null, new Map([["wow", 10]])])'
        for perm in permutations
    ]

    assert result in valid_results


def test_typescript_function_call_typing(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = "test = {'test', True, testing(10)}"
    ts = LANGUAGES["typescript"](pytestconfig)
    result = ts.generate_statement(parse_string(statement_string))
    assert result == 'let test : Set<any> = new Set(["test", true, testing(10)])'

    statement_string = "test = ['test', True, testing(10)]"
    ts = LANGUAGES["typescript"](pytestconfig)
    result = ts.generate_statement(parse_string(statement_string))
    assert result == 'let test : Array<any> = ["test", true, testing(10)]'


def test_javascript_vanilla_object(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "javascript",
        tmp_path,
        "javascript-object.yaml",
        "javascript-object",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_typescript_vanilla_object(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "typescript",
        tmp_path,
        "typescript-object.yaml",
        "typescript-object",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_python_input_prompt_is_ignored(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "one.tson",
        "input-prompt",
    )

    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


def test_haskell_function_arguments_without_brackets(
    tmp_path: Path, pytestconfig: pytest.Config
):
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

    result = generate_statement(bundle, statement)
    assert (
        result == f'{submission_name(bundle.language)}.test 5.5 :: Double "hallo" True'
    )


@pytest.mark.parametrize("lang", ["javascript", "typescript"])
def test_js_ts_exception_correct(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "js-ts-exceptions",
        lang,
        tmp_path,
        "plan.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


def test_javascript_exception_correct_temp(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "js-ts-exceptions",
        "javascript",
        tmp_path,
        "plan.yaml",
        "correct-temp",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
    assert len(updates.find_all("append-message")) == 0


@pytest.mark.parametrize("lang", ["javascript", "typescript"])
def test_js_ts_exception_wrong(lang: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "js-ts-exceptions",
        lang,
        tmp_path,
        "plan.yaml",
        "wrong",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 1


@pytest.mark.parametrize("lang", ["javascript", "typescript"])
def test_js_ts_exception_wrong_null(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "js-ts-exceptions",
        lang,
        tmp_path,
        "plan.yaml",
        "wrong-null",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]
    assert len(updates.find_all("append-message")) == 0


@pytest.mark.parametrize("lang", ["javascript", "typescript"])
def test_js_ts_exception_missing_message(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "js-ts-exceptions",
        lang,
        tmp_path,
        "plan.yaml",
        "wrong-message",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("exercise", ["echo-function-file-input", "echo-function"])
def test_javascript_async(exercise: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, exercise, "javascript", tmp_path, "one.tson", "correct-async"
    )
    workdir = Path(conf.resources).parent / "workdir"
    if workdir.exists():
        shutil.copytree(workdir, tmp_path, dirs_exist_ok=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize("exercise", ["echo-function-file-input", "echo-function"])
def test_typescript_async(exercise: str, tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, exercise, "typescript", tmp_path, "one.tson", "correct-async"
    )
    workdir = Path(conf.resources).parent / "workdir"
    if workdir.exists():
        shutil.copytree(workdir, tmp_path, dirs_exist_ok=True)
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]
