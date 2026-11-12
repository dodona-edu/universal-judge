from pathlib import Path

import pytest
from cattrs import ClassValidationError

from tested.datatypes import BasicNumericTypes
from tested.dsl import parse_dsl, parse_string
from tested.features import Construct
from tested.parsing import get_converter
from tested.serialisation import (
    FunctionCall,
    FunctionType,
    Identifier,
    NumberType,
    PropertyAssignment,
)
from tested.testsuite import (
    ContentPath,
    Context,
    CustomCheckOracle,
    EvaluationFunction,
    ExceptionOutputChannel,
    ExitCodeOutputChannel,
    FileOutputChannel,
    LanguageLiterals,
    LanguageSpecificOracle,
    MainInput,
    Output,
    SupportedLanguage,
    Tab,
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


def test_empty_list_is_not_heterogeneous():
    assert not parse_string("[]").get_used_features().constructs


def test_nested_empty_list_is_not_heterogeneous():
    features = parse_string("[[], [1]]").get_used_features()
    assert Construct.HETEROGENEOUS_COLLECTIONS not in features.constructs


def test_mixed_list_is_heterogeneous():
    features = parse_string('[1, "a"]').get_used_features()
    assert Construct.HETEROGENEOUS_COLLECTIONS in features.constructs


def test_mixed_expected_return_is_not_generated_code():
    suite = """
    - tab: "mixed"
      testcases:
        - expression: "pair_up(1)"
          return: ["a", 1]
    """
    features = parse_dsl(suite).get_suite_features()
    assert Construct.HETEROGENEOUS_COLLECTIONS not in features.generated_code.constructs
    assert Construct.HETEROGENEOUS_COLLECTIONS in features.expected_values.constructs


def test_mixed_argument_is_generated_code():
    suite = """
    - tab: "mixed"
      testcases:
        - expression: "f([1, 'a'])"
          return: 1
    """
    features = parse_dsl(suite).get_suite_features()
    assert Construct.HETEROGENEOUS_COLLECTIONS in features.generated_code.constructs


def test_language_literals_require_at_least_one_literal():
    with pytest.raises(
        ValueError, match="At least one language-specific literal is required."
    ):
        LanguageLiterals(literals={})
    LanguageLiterals(literals={SupportedLanguage.PYTHON: "1"})


def test_custom_check_oracle_accepts_languages():
    function = EvaluationFunction(file=Path("check.py"))
    CustomCheckOracle(function=function, languages={SupportedLanguage.PYTHON})
    CustomCheckOracle(function=function)


def test_custom_check_empty_languages_raises():
    with pytest.raises(ValueError, match="At least one language is required."):
        CustomCheckOracle(
            function=EvaluationFunction(file=Path("evaluator.py")), languages=set()
        )


def test_custom_check_empty_languages_in_json_raises():
    suite = {
        "type": "custom_check",
        "function": {"file": "evaluator.py"},
        "languages": [],
    }
    with pytest.raises(ClassValidationError) as exc_info:
        get_converter().structure(suite, CustomCheckOracle)
    assert "At least one language is required." in str(exc_info.value.exceptions[0])


def test_language_specific_oracle_requires_at_least_one_function():
    with pytest.raises(
        ValueError, match="At least one language-specific oracle is required."
    ):
        LanguageSpecificOracle(functions={})
    LanguageSpecificOracle(
        functions={SupportedLanguage.PYTHON: EvaluationFunction(file=Path("a.py"))}
    )


def test_builtin_value_oracle_requires_a_value():
    with pytest.raises(
        ValueError, match="When using the built-in oracle, a value is required."
    ):
        ValueOutputChannel()
    ValueOutputChannel(value=NumberType(type=BasicNumericTypes.INTEGER, data=1))
    ValueOutputChannel(
        oracle=CustomCheckOracle(function=EvaluationFunction(file=Path("a.py")))
    )


def _tab_with_oracle_files(*files: str) -> Tab:
    testcases = [
        Testcase(
            input=FunctionCall(type=FunctionType.FUNCTION, name="f"),
            output=Output(
                result=ValueOutputChannel(
                    oracle=LanguageSpecificOracle(
                        functions={
                            SupportedLanguage.PYTHON: EvaluationFunction(
                                file=Path(file)
                            )
                        }
                    )
                )
            ),
        )
        for file in files
    ]
    return Tab(name="tab", contexts=[Context(testcases=[tc]) for tc in testcases])


def test_oracle_function_names_must_be_unique_over_files():
    with pytest.raises(
        ValueError, match="Oracle function names must be unique within the same run."
    ):
        _tab_with_oracle_files("a.py", "b.py")
    _tab_with_oracle_files("a.py", "a.py")


def test_property_assignment_requires_a_property():
    with pytest.raises(
        ValueError, match="Assigning to a property requires a property to assign to"
    ):
        PropertyAssignment(
            property=FunctionCall(type=FunctionType.FUNCTION, name="f"),
            expression=Identifier("x"),
        )
    PropertyAssignment(
        property=FunctionCall(type=FunctionType.PROPERTY, name="p"),
        expression=Identifier("x"),
    )
