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
from tested.descriptions.renderer import render_one_statement
from tested.dsl import parse_string
from tested.languages import LANGUAGES
from tested.languages.conventionalize import submission_name
from tested.languages.generation import generate_statement
from tested.languages.haskell import config as haskell_config
from tested.languages.preparation import prepare_assignment, prepare_expression
from tested.languages.python.generators import convert_statement
from tested.serialisation import (
    Assignment,
    BooleanType,
    FunctionCall,
    FunctionType,
    NumberType,
    StringType,
)
from tested.testsuite import Suite
from tests.manual_utils import assert_valid_output, configuration, execute_config


@pytest.mark.csharp
def test_csharp_14_field_keyword(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "echo-function", "csharp", tmp_path, "one.tson", "csharp-14"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.c
def test_c_pointer_char_return(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "c",
        tmp_path,
        "one.tson",
        "wrong-signature",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.c
def test_c_compilation_error_message_with_dots(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo",
        "c",
        tmp_path,
        "one.tson",
        "compile-error-dots",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["compilation error"]
    messages = [
        m if isinstance(m, str) else m["description"]
        for m in (x["message"] for x in updates.find_all("append-message"))
    ]
    assert any("expected declaration specifiers" in m for m in messages)


@pytest.mark.cpp
def test_cpp_error_outside_submission_has_message(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "echo", "cpp", tmp_path, "one.tson", "no-main")
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["compilation error"]
    messages = updates.find_all("append-message")
    descriptions = [m["message"]["description"] for m in messages]
    assert any("error:" in d for d in descriptions)
    assert "<div class='code'></div>" not in descriptions


@pytest.mark.cpp
def test_cpp_negative_char_return(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo-function",
        "cpp",
        tmp_path,
        "one.tson",
        "wrong-signature",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


@pytest.mark.parametrize("submission", ["no-return", "no-return-void"])
@pytest.mark.parametrize("language", ["c", "cpp"])
def test_c_cpp_main_without_return(
    language: str, submission: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "one-exit-code.yaml", submission
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.cpp
def test_cpp_function_try_block_main_without_return(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo", "cpp", tmp_path, "one-exit-code.yaml", "no-return-try"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.parametrize(
    "submission",
    [
        "main-in-comment",
        "main-const-argc",
        "main-comment-before-brace",
        "main-prototype",
        "main-prototype-args",
        "main-ifdef",
        "main-end-macro",
        "void-main",
    ],
)
@pytest.mark.parametrize("language", ["c", "cpp"])
def test_c_cpp_main_forms(
    language: str, submission: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig, "echo", language, tmp_path, "one-exit-code.yaml", submission
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.cpp
def test_cpp_main_trailing_return_type(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "cpp",
        tmp_path,
        "one-exit-code.yaml",
        "main-trailing-return",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]


@pytest.mark.cpp
def test_cpp_function_assignment(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = "test: string = foo()"
    cpp = LANGUAGES["cpp"](pytestconfig)
    result = cpp.generate_statement(parse_string(statement_string))

    assert result == "std::string test = foo()"


@pytest.mark.cpp
def test_cpp_complex_function_assignment(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = 'test = { "a": foo() }'
    cpp = LANGUAGES["cpp"](pytestconfig)
    result = cpp.generate_statement(parse_string(statement_string))

    assert result == 'std::map<std::string, std::any> test = {{"a", foo()}}'


@pytest.mark.cpp
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


@pytest.mark.cpp
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


@pytest.mark.cpp
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


@pytest.mark.cpp
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


@pytest.mark.cpp
@pytest.mark.parametrize("solution", ["crash", "hang"])
def test_cpp_results_survive_crash_and_timeout(
    solution: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "crash-after-result",
        "cpp",
        tmp_path,
        "suite.yaml",
        solution,
        # The linter is off in tests: the limit covers g++ and the run.
        {"time_limit": 10} if solution == "hang" else None,
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum()[0] == "correct"


@pytest.mark.typescript
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


@pytest.mark.typescript
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


@pytest.mark.typescript
def test_typescript_function_call_typing(tmp_path: Path, pytestconfig: pytest.Config):
    statement_string = "test = {'test', True, testing(10)}"
    ts = LANGUAGES["typescript"](pytestconfig)
    result = ts.generate_statement(parse_string(statement_string))
    assert result == 'let test : Set<any> = new Set(["test", true, testing(10)])'

    statement_string = "test = ['test', True, testing(10)]"
    ts = LANGUAGES["typescript"](pytestconfig)
    result = ts.generate_statement(parse_string(statement_string))
    assert result == 'let test : Array<any> = ["test", true, testing(10)]'


@pytest.mark.javascript
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


@pytest.mark.typescript
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


@pytest.mark.python
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


@pytest.mark.haskell
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
        result
        == f'{submission_name(bundle.language)}.test (5.5 :: Double) "hallo" True'
    )


def test_haskell_precompiled_helpers_must_match_templates(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    for module in haskell_config.HELPER_MODULES:
        template = haskell_config.TEMPLATES_DIRECTORY / f"{module}.hs"
        shutil.copy(template, tmp_path)
        for ext in ("hi", "o"):
            (tmp_path / f"{module}.{ext}").write_bytes(b"")

    monkeypatch.setattr(haskell_config, "PRECOMPILED_DIRECTORY", tmp_path)
    haskell_config.precompiled_helpers.cache_clear()
    try:
        assert haskell_config.precompiled_helpers() == tmp_path
        haskell_config.precompiled_helpers.cache_clear()
        with (tmp_path / "Values.hs").open("a") as source:
            source.write("-- outdated\n")
        assert haskell_config.precompiled_helpers() is None
    finally:
        haskell_config.precompiled_helpers.cache_clear()


@pytest.mark.haskell
@pytest.mark.skipif(
    haskell_config.precompiled_helpers() is None,
    reason="The image has no precompiled Haskell helpers",
)
def test_haskell_uses_precompiled_helpers(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "echo-function", "haskell", tmp_path, "one.tson", "correct"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"]

    # The copies keep the modification time of the image, unless GHC recompiled the
    # helpers. Their contents don't tell: GHC would write the same bytes again.
    precompiled = haskell_config.precompiled_helpers()
    assert precompiled
    for module in haskell_config.HELPER_MODULES:
        for ext in ("hi", "o"):
            used = conf.tested_directory / "common" / f"{module}.{ext}"
            shipped = precompiled / f"{module}.{ext}"
            assert used.stat().st_mtime_ns == shipped.stat().st_mtime_ns


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


@pytest.mark.parametrize("lang", ["javascript", "typescript"])
def test_js_ts_module_state_is_fresh_per_context(
    lang: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "counter",
        lang,
        tmp_path,
        "module-state.yaml",
        "module-state",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct", "correct"]


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


@pytest.mark.csharp
def test_csharp_exception_survives_later_exit(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "csharp-exit",
        "csharp",
        tmp_path,
        "plan.yaml",
        "exception-then-exit",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["runtime error", "correct", "correct"]
    generated = [t["generated"] for t in updates.find_all("close-test")]
    assert generated[0] == "InvalidOperationException: boom"


def test_python_large_int_wrong_answer(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "factorial",
        "python",
        tmp_path,
        "plan.yaml",
        "wrong-large-int",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"]


def test_python_nested_namespace(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "nested-namespace",
        "python",
        tmp_path,
        "plan.yaml",
        "correct",
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["correct"] * 7


@pytest.mark.parametrize(
    "expression, expected",
    [
        (
            "[Grocery('Milk', 3, True), MILK]",
            "[submission.Grocery('Milk', 3, True), submission.MILK]",
        ),
        (
            "{Grocery('Milk', 3, True), MILK}",
            "{submission.Grocery('Milk', 3, True), submission.MILK}",
        ),
        (
            "(Grocery('Milk', 3, True), MILK)",
            "(submission.Grocery('Milk', 3, True), submission.MILK)",
        ),
        (
            "{'milk': Grocery('Milk', 3, True), 'global': MILK}",
            "{'milk': submission.Grocery('Milk', 3, True), 'global': submission.MILK}",
        ),
        (
            "[[Grocery('Milk', 3, True)], {'milk': MILK}]",
            "[[submission.Grocery('Milk', 3, True)], {'milk': submission.MILK}]",
        ),
        (
            "groceries = [Grocery('Milk', 3, True), MILK]",
            "groceries = [submission.Grocery('Milk', 3, True), submission.MILK]",
        ),
        (
            "basket_total(Basket([MILK]))",
            "submission.basket_total(submission.Basket([submission.MILK]))",
        ),
    ],
)
def test_python_nested_values_keep_submission_namespace(
    expression: str, expected: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, Suite())

    statement = parse_string(expression)
    if isinstance(statement, Assignment):
        statement = prepare_assignment(bundle, statement)
    else:
        statement = prepare_expression(bundle, statement)

    assert convert_statement(statement, True) == expected


@pytest.mark.parametrize(
    "language",
    [
        pytest.param("javascript", marks=pytest.mark.javascript),
        pytest.param("typescript", marks=pytest.mark.typescript),
    ],
)
def test_javascript_description_keeps_await_in_strings(
    language: str, tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "", language, tmp_path)
    bundle = create_bundle(conf, sys.stdout, Suite())
    statement = 'echo("await you")'
    assert render_one_statement(bundle, statement) == statement


@pytest.mark.parametrize(
    "language, source, name, void_name",
    [
        pytest.param(
            "c",
            "int total(int a, int b) {\n    return a + b;\n}\n\n"
            "void total_all(int* values, int length) {\n}\n",
            "total",
            "total_all",
            marks=pytest.mark.c,
            id="c",
        ),
        pytest.param(
            "cpp",
            "int total(int a, int b) {\n    return a + b;\n}\n\n"
            "void total_all(std::vector<int> values) {\n}\n",
            "total",
            "total_all",
            marks=pytest.mark.cpp,
            id="cpp",
        ),
        pytest.param(
            "csharp",
            "class Submission\n{\n"
            "    public static int Total(int a, int b)\n    {\n"
            "        return a + b;\n    }\n\n"
            "    public static void TotalAll(int[] values)\n    {\n    }\n}\n",
            "Total",
            "TotalAll",
            marks=pytest.mark.csharp,
            id="csharp",
        ),
        pytest.param(
            "java",
            "class Submission {\n"
            "    public static int total(int a, int b) {\n        return a + b;\n    }\n\n"
            "    public static void totalAll(int[] values) {\n    }\n}\n",
            "total",
            "totalAll",
            marks=pytest.mark.java,
            id="java",
        ),
    ],
)
def test_is_void_method_matches_whole_names(
    language: str,
    source: str,
    name: str,
    void_name: str,
    tmp_path: Path,
    pytestconfig: pytest.Config,
):
    submission = tmp_path / "submission.txt"
    submission.write_text(source)
    conf = configuration(
        pytestconfig, "", language, tmp_path, options={"source": submission}
    )
    bundle = create_bundle(conf, sys.stdout, Suite())

    assert not bundle.language.is_void_method(name)
    assert bundle.language.is_void_method(void_name)


@pytest.mark.parametrize(
    "language, source",
    [
        pytest.param(
            "c",
            "void test\u0301(int* values, int length) {\n}\n",
            marks=pytest.mark.c,
            id="c",
        ),
        pytest.param(
            "cpp",
            "void test\u0301(std::vector<int> values) {\n}\n",
            marks=pytest.mark.cpp,
            id="cpp",
        ),
        pytest.param(
            "csharp",
            "class Submission\n{\n"
            "    public static void test\u0301(int[] values)\n    {\n    }\n}\n",
            marks=pytest.mark.csharp,
            id="csharp",
        ),
        pytest.param(
            "java",
            "class Submission {\n"
            "    public static void test\u0301(int[] values) {\n    }\n}\n",
            marks=pytest.mark.java,
            id="java",
        ),
    ],
)
def test_is_void_method_matches_names_ending_in_combining_mark(
    language: str,
    source: str,
    tmp_path: Path,
    pytestconfig: pytest.Config,
):
    submission = tmp_path / "submission.txt"
    submission.write_text(source)
    conf = configuration(
        pytestconfig, "", language, tmp_path, options={"source": submission}
    )
    bundle = create_bundle(conf, sys.stdout, Suite())

    assert bundle.language.is_void_method("test\u0301")
