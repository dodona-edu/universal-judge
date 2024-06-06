from tested.parsing import get_converter
from tested.serialisation import FunctionCall, FunctionType
from tested.testsuite import (
    CustomCheckOracle,
    ExceptionOutputChannel,
    ExitCodeOutputChannel,
    FileOutputChannel,
    MainInput,
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
    assert result.stdin.data == "input-1"


def test_text_show_expected_is_accepted():
    scheme = """
    {
        "show_expected": true,
        "data": "hallo",
        "type": "text"
    }
    """
    result = get_converter().loads(scheme, TextOutputChannel)
    assert result.data == "hallo"


def test_file_show_expected_is_accepted():
    scheme = """
    {
        "show_expected": true,
        "expected_path": "hallo",
        "actual_path": "hallo"
    }
    """
    result = get_converter().loads(scheme, FileOutputChannel)
    assert result.expected_path == "hallo"
    assert result.actual_path == "hallo"


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


def test_generic_functions_are_kept():
    scheme = """
    {
        "type": "function",
        "name": "__TeSt__"
    }
    """
    result = get_converter().loads(scheme, FunctionCall)
    assert result.type == FunctionType.FUNCTION
    assert result.name == "TeSt"
    assert result.name.is_raw
