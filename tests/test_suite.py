import pytest

from tested.parsing import get_converter
from tested.serialisation import FunctionCall, FunctionType
from tested.testsuite import (
    ContentPath,
    Context,
    CustomCheckOracle,
    ExceptionOutputChannel,
    ExitCodeOutputChannel,
    FileOutputChannel,
    MainInput,
    Output,
    Testcase,
    TextData,
    TextOutputChannel,
    ValueOutputChannel,
)


def test_text_output_is_compatible_oracle():
    old_structure = {
        "evaluator": {
            "function": {"file": "evaluate.py"},
            "type": "custom_check",
        },
        "data": "example",
    }

    result = get_converter().structure(old_structure, TextOutputChannel)

    assert isinstance(result.oracle, CustomCheckOracle)
    assert result.oracle.function.file.name == "evaluate.py"


def test_file_output_is_compatible_oracle():
    old_structure = {
        "evaluator": {
            "function": {"file": "evaluate.py"},
            "type": "custom_check",
        },
        "expected_path": "one.py",
        "actual_path": "two.py",
    }

    result = get_converter().structure(old_structure, FileOutputChannel)

    assert isinstance(result.oracle, CustomCheckOracle)
    assert result.oracle.function.file.name == "evaluate.py"


def test_value_output_is_compatible_oracle():
    old_structure = {
        "evaluator": {
            "function": {"file": "evaluate.py"},
            "type": "custom_check",
        },
        "value": {"type": "text", "data": "yes"},
    }

    result = get_converter().structure(old_structure, ValueOutputChannel)

    assert isinstance(result.oracle, CustomCheckOracle)
    assert result.oracle.function.file.name == "evaluate.py"


def test_exception_output_is_compatible_oracle():
    old_structure = {
        "evaluator": {
            "function": {"file": "evaluate.py"},
            "type": "custom_check",
        },
        "exception": {"message": "Yes", "types": {"python": "yes"}},
    }

    result = get_converter().structure(old_structure, ValueOutputChannel)

    assert isinstance(result.oracle, CustomCheckOracle)
    assert result.oracle.function.file.name == "evaluate.py"


def test_input_deprecated_attribute_is_accepted():
    scheme = """
    {
        "main_call": true,
        "stdin": {
            "type": "text",
            "data": "input-1"
        }
    }
    """
    result = get_converter().loads(scheme, MainInput)
    assert isinstance(result.stdin, TextData)
    assert result.stdin.content == "input-1"


def test_text_show_expected_is_accepted():
    scheme = """
    {
        "show_expected": true,
        "data": "hallo",
        "type": "text"
    }
    """
    result = get_converter().loads(scheme, TextOutputChannel)
    assert result.content == "hallo"


def test_file_show_expected_is_accepted():
    scheme = """
    {
        "show_expected": true,
        "expected_path": "expected-hallo",
        "actual_path": "actual-hallo"
    }
    """
    result = get_converter().loads(scheme, FileOutputChannel)
    assert result.files[0].path == "actual-hallo"
    assert result.files[0].content == ContentPath(path="expected-hallo")


def test_value_show_expected_is_accepted():
    scheme = """
    {
        "show_expected": true,
        "value": {
            "type": "text",
            "data": "yes"
        }
    }
    """
    result = get_converter().loads(scheme, ValueOutputChannel)
    assert result.value
    assert result.value.data == "yes"


def test_exception_show_expected_is_accepted():
    scheme = """
    {
        "show_expected": true,
        "exception": {
            "message": "text"
        }
    }
    """
    result = get_converter().loads(scheme, ExceptionOutputChannel)
    assert result.exception
    assert result.exception.message == "text"


def test_exit_show_expected_is_accepted():
    scheme = """
    {
        "show_expected": true,
        "value": 0
    }
    """
    result = get_converter().loads(scheme, ExitCodeOutputChannel)
    assert result.value == 0


def test_only_first_testcase_may_have_main_call():
    with pytest.raises(
        ValueError, match="Only the first testcase may have a main call."
    ):
        Context(
            testcases=[
                Testcase(input=FunctionCall(type=FunctionType.FUNCTION, name="f")),
                Testcase(input=MainInput()),
            ]
        )


def test_non_last_testcase_may_not_check_exit_code():
    with pytest.raises(
        ValueError, match="Only the last testcase may have an exit code check."
    ):
        Context(
            testcases=[
                Testcase(input=FunctionCall(type=FunctionType.FUNCTION, name="f")),
                Testcase(
                    input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                    output=Output(exit_code=ExitCodeOutputChannel(value=0)),
                ),
                Testcase(input=FunctionCall(type=FunctionType.FUNCTION, name="f")),
            ]
        )


def test_input_files_same_path_same_content_is_valid():
    Context(
        testcases=[
            Testcase(
                input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                input_files=[TextData(content="hello", path="in.txt")],
            ),
            Testcase(
                input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                input_files=[TextData(content="hello", path="in.txt")],
            ),
        ]
    )
    assert True, "Did not raise"


def test_input_files_same_path_different_content_raises():
    with pytest.raises(
        ValueError, match="The same path with different content is used in input_files"
    ):
        Context(
            testcases=[
                Testcase(
                    input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                    input_files=[TextData(content="hello", path="in.txt")],
                ),
                Testcase(
                    input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                    input_files=[TextData(content="world", path="in.txt")],
                ),
            ]
        )


def test_output_files_same_path_same_content_is_valid():
    Context(
        testcases=[
            Testcase(
                input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                output=Output(
                    file=FileOutputChannel(
                        files=[TextData(content="x", path="out.txt")]
                    )
                ),
            ),
            Testcase(
                input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                output=Output(
                    file=FileOutputChannel(
                        files=[TextData(content="x", path="out.txt")]
                    )
                ),
            ),
        ]
    )
    assert True, "Did not raise"


def test_output_files_same_path_different_content_raises():
    with pytest.raises(
        ValueError, match="The same path with different content is used in output_files"
    ):
        Context(
            testcases=[
                Testcase(
                    input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                    output=Output(
                        file=FileOutputChannel(
                            files=[TextData(content="x", path="out.txt")]
                        )
                    ),
                ),
                Testcase(
                    input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
                    output=Output(
                        file=FileOutputChannel(
                            files=[TextData(content="y", path="out.txt")]
                        )
                    ),
                ),
            ]
        )
