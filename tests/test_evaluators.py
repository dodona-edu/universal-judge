import sys
from pathlib import Path
from unittest.mock import ANY

import tested
from tested.configs import create_bundle
from tested.datatypes import BasicObjectTypes, BasicSequenceTypes, BasicStringTypes
from tested.dodona import Status
from tested.evaluators.common import EvaluationResult, EvaluatorConfig
from tested.evaluators.exception import evaluate as evaluate_exception
from tested.evaluators.text import evaluate_file, evaluate_text
from tested.evaluators.value import evaluate as evaluate_value
from tested.parsing import get_converter
from tested.serialisation import (
    ExceptionValue,
    ObjectKeyValuePair,
    ObjectType,
    SequenceType,
    StringType,
)
from tested.testsuite import (
    ExceptionOutputChannel,
    ExpectedException,
    FileOutputChannel,
    Suite,
    TextOutputChannel,
    ValueOutputChannel,
)
from tests.manual_utils import configuration


def evaluator_config(
    tmp_path: Path, pytestconfig, options=None, language="python"
) -> EvaluatorConfig:
    if options is None:
        options = dict()
    conf = configuration(pytestconfig, "", language, tmp_path)
    plan = Suite()
    bundle = create_bundle(conf, sys.stdout, plan)
    return EvaluatorConfig(bundle=bundle, options=options, context_dir=tmp_path)


def test_text_evaluator(tmp_path: Path, pytestconfig):
    config = evaluator_config(tmp_path, pytestconfig, {"ignoreWhitespace": False})
    channel = TextOutputChannel(data="expected")
    result = evaluate_text(config, channel, "expected")
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected"
    assert result.readable_actual == "expected"

    result = evaluate_text(config, channel, "nothing")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "expected"
    assert result.readable_actual == "nothing"


def test_text_evaluator_whitespace(tmp_path: Path, pytestconfig):
    config = evaluator_config(tmp_path, pytestconfig, {"ignoreWhitespace": True})
    channel = TextOutputChannel(data="expected")
    result = evaluate_text(config, channel, "expected      ")
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected"
    assert result.readable_actual == "expected      "

    result = evaluate_text(config, channel, "nothing")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "expected"
    assert result.readable_actual == "nothing"


def test_text_evaluator_case_sensitive(tmp_path: Path, pytestconfig):
    config = evaluator_config(tmp_path, pytestconfig, {"caseInsensitive": True})
    channel = TextOutputChannel(data="expected")
    result = evaluate_text(config, channel, "Expected")
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected"
    assert result.readable_actual == "Expected"

    result = evaluate_text(config, channel, "nothing")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "expected"
    assert result.readable_actual == "nothing"


def test_text_evaluator_combination(tmp_path: Path, pytestconfig):
    config = evaluator_config(
        tmp_path, pytestconfig, {"caseInsensitive": True, "ignoreWhitespace": True}
    )
    channel = TextOutputChannel(data="expected")
    result = evaluate_text(config, channel, "Expected     ")
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected"
    assert result.readable_actual == "Expected     "

    result = evaluate_text(config, channel, "nothing")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "expected"
    assert result.readable_actual == "nothing"


def test_text_evaluator_rounding(tmp_path: Path, pytestconfig):
    config = evaluator_config(
        tmp_path, pytestconfig, {"tryFloatingPoint": True, "applyRounding": True}
    )
    channel = TextOutputChannel(data="1.333")
    result = evaluate_text(config, channel, "1.3333333")
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "1.333"
    assert result.readable_actual == "1.3333333"

    result = evaluate_text(config, channel, "1.5")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "1.333"
    assert result.readable_actual == "1.5"


def test_text_evaluator_round_to(tmp_path: Path, pytestconfig):
    config = evaluator_config(
        tmp_path,
        pytestconfig,
        {"tryFloatingPoint": True, "applyRounding": True, "roundTo": 1},
    )
    channel = TextOutputChannel(data="1.3")
    result = evaluate_text(config, channel, "1.3333333")
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "1.3"
    assert result.readable_actual == "1.3333333"

    result = evaluate_text(config, channel, "1.5")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "1.3"
    assert result.readable_actual == "1.5"


def test_file_evaluator_full_wrong(tmp_path: Path, pytestconfig, mocker):
    config = evaluator_config(tmp_path, pytestconfig, {"mode": "full"})
    s = mocker.spy(tested.evaluators.text, name="compare_text")
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in ["expected\nexpected", "actual\nactual"]
    ]
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)
    channel = FileOutputChannel(
        expected_path="expected.txt", actual_path="expected.txt"
    )
    result = evaluate_file(config, channel, "")
    s.assert_called_once_with(ANY, "expected\nexpected", "actual\nactual")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "expected\nexpected"
    assert result.readable_actual == "actual\nactual"


def test_file_evaluator_full_correct(tmp_path: Path, pytestconfig, mocker):
    config = evaluator_config(tmp_path, pytestconfig, {"mode": "full"})
    s = mocker.spy(tested.evaluators.text, name="compare_text")
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in ["expected\nexpected", "expected\nexpected"]
    ]
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)
    channel = FileOutputChannel(
        expected_path="expected.txt", actual_path="expected.txt"
    )
    result = evaluate_file(config, channel, "")
    s.assert_called_once_with(ANY, "expected\nexpected", "expected\nexpected")
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected\nexpected"
    assert result.readable_actual == "expected\nexpected"


def test_file_evaluator_line_wrong(tmp_path: Path, pytestconfig, mocker):
    config = evaluator_config(
        tmp_path, pytestconfig, {"mode": "line", "stripNewlines": True}
    )
    s = mocker.spy(tested.evaluators.text, name="compare_text")
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in ["expected\nexpected2", "actual\nactual2"]
    ]
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)
    channel = FileOutputChannel(
        expected_path="expected.txt", actual_path="expected.txt"
    )
    result = evaluate_file(config, channel, "")
    s.assert_any_call(ANY, "expected", "actual")
    s.assert_any_call(ANY, "expected2", "actual2")
    assert s.call_count == 2
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "expected\nexpected2"
    assert result.readable_actual == "actual\nactual2"


def test_file_evaluator_line_correct(tmp_path: Path, pytestconfig, mocker):
    config = evaluator_config(
        tmp_path, pytestconfig, {"mode": "line", "stripNewlines": True}
    )
    s = mocker.spy(tested.evaluators.text, name="compare_text")
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in ["expected\nexpected2", "expected\nexpected2"]
    ]
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)
    channel = FileOutputChannel(
        expected_path="expected.txt", actual_path="expected.txt"
    )
    result = evaluate_file(config, channel, "")
    s.assert_any_call(ANY, "expected", "expected")
    s.assert_any_call(ANY, "expected2", "expected2")
    assert s.call_count == 2
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected\nexpected2"
    assert result.readable_actual == "expected\nexpected2"


def test_file_evaluator_strip_lines_correct(tmp_path: Path, pytestconfig, mocker):
    config = evaluator_config(
        tmp_path, pytestconfig, {"mode": "line", "stripNewlines": True}
    )
    s = mocker.spy(tested.evaluators.text, name="compare_text")
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in ["expected\nexpected2\n", "expected\nexpected2"]
    ]
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)
    channel = FileOutputChannel(
        expected_path="expected.txt", actual_path="expected.txt"
    )
    result = evaluate_file(config, channel, "")
    s.assert_any_call(ANY, "expected", "expected")
    s.assert_any_call(ANY, "expected2", "expected2")
    assert s.call_count == 2
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected\nexpected2\n"
    assert result.readable_actual == "expected\nexpected2"


def test_file_evaluator_dont_strip_lines_correct(tmp_path: Path, pytestconfig, mocker):
    config = evaluator_config(
        tmp_path, pytestconfig, {"mode": "line", "stripNewlines": False}
    )
    s = mocker.spy(tested.evaluators.text, name="compare_text")
    mock_files = [
        mocker.mock_open(read_data=content).return_value
        for content in ["expected\nexpected2\n", "expected\nexpected2"]
    ]
    mock_opener = mocker.mock_open()
    mock_opener.side_effect = mock_files
    mocker.patch("builtins.open", mock_opener)
    channel = FileOutputChannel(
        expected_path="expected.txt", actual_path="expected.txt"
    )
    result = evaluate_file(config, channel, "")
    s.assert_any_call(ANY, "expected\n", "expected\n")
    s.assert_any_call(ANY, "expected2\n", "expected2")
    assert s.call_count == 2
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "expected\nexpected2\n"
    assert result.readable_actual == "expected\nexpected2"


def test_exception_evaluator_only_messages_correct(tmp_path: Path, pytestconfig):
    config = evaluator_config(tmp_path, pytestconfig)
    channel = ExceptionOutputChannel(exception=ExpectedException(message="Test error"))
    actual_value = get_converter().dumps(
        ExceptionValue(message="Test error", type="ZeroDivisionError"),
    )
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "ZeroDivisionError: Test error"
    assert result.readable_actual == "ZeroDivisionError: Test error"


def test_exception_evaluator_only_messages_wrong(tmp_path: Path, pytestconfig):
    config = evaluator_config(tmp_path, pytestconfig)
    channel = ExceptionOutputChannel(exception=ExpectedException(message="Test error"))
    actual_value = get_converter().dumps(
        ExceptionValue(message="Pief poef", type="ZeroDivisionError"),
    )
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "Test error"
    assert result.readable_actual == "Pief poef"


def test_exception_evaluator_correct_message_wrong_type(tmp_path: Path, pytestconfig):
    channel = ExceptionOutputChannel(
        exception=ExpectedException(
            message="Test error",
            types={"python": "PiefError", "javascript": "PafError"},
        )
    )
    actual_value = get_converter().dumps(
        ExceptionValue(message="Test error", type="ZeroDivisionError"),
    )

    # Test for Python
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "PiefError: Test error"
    assert result.readable_actual == "ZeroDivisionError: Test error"

    # Test for JavaScript
    config = evaluator_config(tmp_path, pytestconfig, language="javascript")
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "PafError: Test error"
    assert result.readable_actual == "ZeroDivisionError: Test error"


def test_exception_evaluator_wrong_message_correct_type(tmp_path: Path, pytestconfig):
    channel = ExceptionOutputChannel(
        exception=ExpectedException(
            message="Test error",
            types={"python": "PiefError", "javascript": "PafError"},
        )
    )

    # Test for Python
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    actual_value = get_converter().dumps(
        ExceptionValue(message="Test errors", type="PiefError"),
    )
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "PiefError: Test error"
    assert result.readable_actual == "PiefError: Test errors"

    # Test for JavaScript
    config = evaluator_config(tmp_path, pytestconfig, language="javascript")
    actual_value = get_converter().dumps(
        ExceptionValue(message="Test errors", type="PafError")
    )
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "PafError: Test error"
    assert result.readable_actual == "PafError: Test errors"


def test_exception_evaluator_correct_type_and_message(tmp_path: Path, pytestconfig):
    channel = ExceptionOutputChannel(
        exception=ExpectedException(
            message="Test error",
            types={"python": "PiefError", "javascript": "PafError"},
        )
    )

    # Test for Python
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    actual_value = get_converter().dumps(
        ExceptionValue(message="Test error", type="PiefError")
    )
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "PiefError: Test error"
    assert result.readable_actual == "PiefError: Test error"

    # Test for JavaScript
    config = evaluator_config(tmp_path, pytestconfig, language="javascript")
    actual_value = get_converter().dumps(
        ExceptionValue(message="Test error", type="PafError")
    )
    result = evaluate_exception(config, channel, actual_value)
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "PafError: Test error"
    assert result.readable_actual == "PafError: Test error"


def test_value_string_as_text_is_detected(tmp_path: Path, pytestconfig):
    channel = ValueOutputChannel(
        value=StringType(type=BasicStringTypes.TEXT, data="multi\nline\nstring")
    )
    actual_value = get_converter().dumps(
        StringType(type=BasicStringTypes.TEXT, data="multi\nline\nstring")
    )
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    result = evaluate_value(config, channel, actual_value)
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "multi\nline\nstring"
    assert result.readable_actual == "multi\nline\nstring"


def test_value_string_as_text_is_not_detected_if_disabled(tmp_path: Path, pytestconfig):
    channel = ValueOutputChannel(
        value=StringType(type=BasicStringTypes.TEXT, data="multi\nline\nstring")
    )
    actual_value = get_converter().dumps(
        StringType(type=BasicStringTypes.TEXT, data="multi\nline\nstring")
    )
    config = evaluator_config(
        tmp_path, pytestconfig, language="python", options={"stringsAsText": False}
    )
    result = evaluate_value(config, channel, actual_value)
    assert result.result.enum == Status.CORRECT
    assert result.readable_expected == "'multi\\nline\\nstring'"
    assert result.readable_actual == "'multi\\nline\\nstring'"


def test_value_string_as_text_is_not_detected_if_not_multiline(
    tmp_path: Path, pytestconfig
):
    channel = ValueOutputChannel(
        value=StringType(type=BasicStringTypes.TEXT, data="multi")
    )
    actual_value = get_converter().dumps(
        StringType(type=BasicStringTypes.TEXT, data="multi\nline\nstring"),
    )
    config = evaluator_config(
        tmp_path, pytestconfig, language="python", options={"stringsAsText": False}
    )
    result = evaluate_value(config, channel, actual_value)
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "'multi'"
    assert result.readable_actual == "'multi\\nline\\nstring'"


def test_value_string_as_text_is_detected_when_no_actual(tmp_path: Path, pytestconfig):
    channel = ValueOutputChannel(
        value=StringType(type=BasicStringTypes.TEXT, data="multi\nline\nstring")
    )
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    result = evaluate_value(config, channel, "")
    assert result.result.enum == Status.WRONG
    assert result.readable_expected == "multi\nline\nstring"
    assert result.readable_actual == ""


def test_nested_sets_type_check_works_if_correct(tmp_path: Path, pytestconfig):
    expected_value = SequenceType(
        type=BasicSequenceTypes.SET,
        data=[
            SequenceType(
                type=BasicSequenceTypes.SET,
                data=[
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                    StringType(type=BasicStringTypes.TEXT, data="b"),
                ],
            ),
            SequenceType(
                type=BasicSequenceTypes.SET,
                data=[
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                ],
            ),
            StringType(type=BasicStringTypes.TEXT, data="c"),
        ],
    )
    actual_value = SequenceType(
        type=BasicSequenceTypes.SET,
        data=[
            SequenceType(
                type=BasicSequenceTypes.SET,
                data=[
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                ],
            ),
            StringType(type=BasicStringTypes.TEXT, data="c"),
            SequenceType(
                type=BasicSequenceTypes.SET,
                data=[
                    StringType(type=BasicStringTypes.TEXT, data="b"),
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                ],
            ),
        ],
    )
    channel = ValueOutputChannel(value=expected_value)
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    result = evaluate_value(config, channel, get_converter().dumps(actual_value))
    assert result.result.enum == Status.CORRECT


def test_too_many_sequence_values_dont_crash(tmp_path: Path, pytestconfig):
    expected_value = SequenceType(
        type=BasicSequenceTypes.SEQUENCE,
        data=[
            SequenceType(
                type=BasicSequenceTypes.SET,
                data=[
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                    StringType(type=BasicStringTypes.TEXT, data="b"),
                ],
            ),
        ],
    )
    actual_value = SequenceType(
        type=BasicSequenceTypes.SEQUENCE,
        data=[
            SequenceType(
                type=BasicSequenceTypes.SET,
                data=[
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                ],
            ),
            StringType(type=BasicStringTypes.TEXT, data="c"),
            SequenceType(
                type=BasicSequenceTypes.SET,
                data=[
                    StringType(type=BasicStringTypes.TEXT, data="b"),
                    StringType(type=BasicStringTypes.TEXT, data="a"),
                ],
            ),
        ],
    )
    channel = ValueOutputChannel(value=expected_value)
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    result = evaluate_value(config, channel, get_converter().dumps(actual_value))
    assert result.result.enum == Status.WRONG


def test_too_many_object_values_dont_crash(tmp_path: Path, pytestconfig):
    expected_value = ObjectType(
        type=BasicObjectTypes.MAP,
        data=[
            ObjectKeyValuePair(
                key=StringType(type=BasicStringTypes.TEXT, data="a"),
                value=StringType(type=BasicStringTypes.TEXT, data="b"),
            )
        ],
    )
    actual_value = ObjectType(
        type=BasicObjectTypes.MAP,
        data=[
            ObjectKeyValuePair(
                key=StringType(type=BasicStringTypes.TEXT, data="a"),
                value=StringType(type=BasicStringTypes.TEXT, data="b"),
            ),
            ObjectKeyValuePair(
                key=StringType(type=BasicStringTypes.TEXT, data="c"),
                value=StringType(type=BasicStringTypes.TEXT, data="d"),
            ),
        ],
    )
    channel = ValueOutputChannel(value=expected_value)
    config = evaluator_config(tmp_path, pytestconfig, language="python")
    result = evaluate_value(
        config,
        channel,
        get_converter().dumps(actual_value),
    )
    assert result.result.enum == Status.WRONG
