import pytest
from attrs import define

from tested.parsing import fallback_field, get_converter, ignore_field, parse_json_suite
from tested.serialisation import Identifier, StringType
from tested.testsuite import (
    ContentPath,
    Context,
    ExceptionBuiltin,
    ExceptionOutputChannel,
    FileOutputChannel,
    GenericExceptionOracle,
    GenericTextOracle,
    GenericValueOracle,
    LanguageSpecificOracle,
    MainInput,
    SupportedLanguage,
    Tab,
    Testcase,
    TextBuiltin,
    TextData,
    TextOutputChannel,
    ValueBuiltin,
    ValueOutputChannel,
)


def test_language_specific_oracle_legacy_evaluators():
    converter = get_converter()
    data = {
        "evaluators": {"python": {"file": "test.py", "name": "test"}},
        "arguments": {"python": ["arg1"]},
    }
    result = converter.structure(data, LanguageSpecificOracle)
    assert "python" in result.functions
    assert result.functions[SupportedLanguage.PYTHON].name == "test"
    assert result.arguments[SupportedLanguage.PYTHON] == ["arg1"]


def test_text_data_legacy_data_string():
    data = {"data": "some content"}
    result = get_converter().structure(data, TextData)
    assert result.content == "some content"


def test_text_data_legacy_data_file():
    data = {"data": "path/to/file.txt", "type": "file"}
    result = get_converter().structure(data, TextData)

    assert isinstance(result.content, ContentPath)
    assert result.content.path == "path/to/file.txt"


def test_text_output_channel_legacy_evaluator():
    data = {"data": "expected", "evaluator": {"name": "text"}}
    result = get_converter().structure(data, TextOutputChannel)
    assert result.content == "expected"
    assert isinstance(result.oracle, GenericTextOracle)
    assert result.oracle.name == TextBuiltin.TEXT


def test_file_output_channel_legacy_evaluator():
    data = {
        "expected_path": "exp.txt",
        "actual_path": "act.txt",
        "evaluator": {"name": "file"},
    }
    result = get_converter().structure(data, FileOutputChannel)
    assert result.expected_path == "exp.txt"
    assert result.actual_path == "act.txt"
    assert isinstance(result.oracle, GenericTextOracle)
    assert result.oracle.name == TextBuiltin.FILE


def test_value_output_channel_legacy_evaluator():
    data = {"value": {"type": "text", "data": "val"}, "evaluator": {"name": "value"}}
    result = get_converter().structure(data, ValueOutputChannel)
    assert isinstance(result.value, StringType)
    assert isinstance(result.oracle, GenericValueOracle)
    assert result.oracle.name == ValueBuiltin.VALUE


def test_exception_output_channel_legacy_evaluator():
    data = {"exception": {"message": "error"}, "evaluator": {"name": "exception"}}
    result = get_converter().structure(data, ExceptionOutputChannel)
    assert result.exception is not None
    assert result.exception.message == "error"
    assert isinstance(result.oracle, GenericExceptionOracle)
    assert result.oracle.name == ExceptionBuiltin.EXCEPTION


def test_tab_legacy_runs():
    data = {
        "name": "Tab 1",
        "runs": [
            {"run": {"input": {"arguments": ["arg1"], "main_call": True}}},
            {"contexts": [{"testcases": [{"input": "5"}]}]},
        ],
    }
    result = get_converter().structure(data, Tab)
    assert len(result.contexts) == 2
    assert isinstance(result.contexts[0].testcases[0].input, MainInput)
    assert result.contexts[0].testcases[0].input.arguments == ["arg1"]

    assert isinstance(result.contexts[1].testcases[0].input, Identifier)
    assert len(result.contexts[1].testcases[0].input) == 1
    assert result.contexts[1].testcases[0].input == "5"


def test_ignore_fields():
    converter = get_converter()

    # TextData ignores 'type' (used in converter, but should be popped)
    data_text = {"content": "text", "type": "file"}
    result_text = converter.structure(data_text, TextData)
    assert result_text.content == "text"

    # Testcase ignores 'essential'
    data_testcase = {"input": {"arguments": [], "main_call": True}, "essential": True}
    result_testcase = converter.structure(data_testcase, Testcase)
    assert isinstance(result_testcase.input, MainInput)

    # Context ignores 'link_files'
    data_context = {"testcases": [], "link_files": []}
    result_context = converter.structure(data_context, Context)
    assert result_context.testcases == []


def test_full_suite_legacy_format():
    json_suite = """
    {
      "namespace": "test",
      "tabs": [
        {
          "name": "Tab 1",
          "runs": [
            {
              "run": {
                "input": {
                  "arguments": ["a"],
                  "main_call": true
                },
                "output": {
                   "stdout": {
                     "data": "out",
                     "evaluator": {"name": "text"}
                   }
                }
              }
            }
          ]
        }
      ]
    }
    """
    suite = parse_json_suite(json_suite)
    assert suite.namespace == "test"
    assert len(suite.tabs) == 1
    assert suite.tabs[0].name == "Tab 1"
    assert len(suite.tabs[0].contexts) == 1
    tc = suite.tabs[0].contexts[0].testcases[0]
    assert isinstance(tc.input, MainInput)
    assert isinstance(tc.output.stdout, TextOutputChannel)
    assert tc.output.stdout.content == "out"
    assert isinstance(tc.output.stdout.oracle, GenericTextOracle)


@fallback_field({"old": "new"})
@ignore_field("old")
@define
class OrderTest:
    new: str


def test_decorator_order():
    # If fallback runs first, "old" is mapped to "new", then "old" is ignored.
    # If ignore runs first, "old" is removed, then fallback sees nothing.
    data = {"old": "value"}
    result = get_converter().structure(data, OrderTest)
    assert result.new == "value"


@ignore_field("old")
@fallback_field({"old": "new"})
@define
class ReverseOrderTest:
    new: str


def test_reverse_decorator_order():
    data = {"old": "value"}
    # If ignore runs first, this should fail because "new" is missing.
    with pytest.raises(Exception):
        get_converter().structure(data, ReverseOrderTest)
