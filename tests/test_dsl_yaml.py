import json
from pathlib import Path

import pytest

from tested.datatypes import (
    AdvancedNumericTypes,
    AdvancedSequenceTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BooleanTypes,
    NothingTypes,
    NumericTypes,
    ObjectTypes,
    SequenceTypes,
    StringTypes,
)
from tested.dsl import parse_dsl, translate_to_test_suite
from tested.serialisation import (
    FunctionCall,
    NumberType,
    ObjectType,
    SequenceType,
    StringType,
    VariableAssignment,
)
from tested.testsuite import (
    CustomCheckOracle,
    FileUrl,
    GenericTextOracle,
    GenericValueOracle,
    LanguageLiterals,
    TextOutputChannel,
    ValueOutputChannel,
    parse_test_suite,
)
from tested.utils import get_args


def test_parse_one_tab_ctx():
    yaml_str = """
namespace: "solution"
tabs:
- tab: "Ctx"
  testcases:
  - arguments: [ "--arg", "argument" ]
    stdin: "Input string"
    stdout: "Output string"
    stderr: "Error string"
    exit_code: 1
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert suite.namespace == "solution"
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert tab.name == "Ctx"
    assert len(tab.contexts) == 1
    context = tab.contexts[0]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.is_main_testcase()
    assert tc.input.stdin.data == "Input string\n"
    assert tc.input.arguments == ["--arg", "argument"]
    assert tc.output.stderr.data == "Error string\n"
    assert tc.output.stdout.data == "Output string\n"
    assert tc.output.exit_code.value == 1


def test_parse_ctx_exception():
    yaml_str = """
- tab: "Ctx Exception"
  hidden: false
  testcases:
  - arguments: [ "--arg", "fail" ]
    exception: "Exception message"
  - arguments: [ "--arg", "fail2" ]
    exit_code: 10
- tab: "Ctx Error"
  testcases:
  - arguments: [ "--arg", "error" ]
    exception: "Error"
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 2
    tab = suite.tabs[0]
    assert not tab.hidden
    assert tab.name == "Ctx Exception"
    assert len(tab.contexts) == 2
    context = tab.contexts[0]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.input.arguments == ["--arg", "fail"]
    assert tc.output.exception.exception.message == "Exception message"
    context = tab.contexts[1]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.input.arguments == ["--arg", "fail2"]
    assert tc.output.exit_code.value == 10
    tab = suite.tabs[1]
    assert tab.name == "Ctx Error"
    assert len(tab.contexts) == 1
    context = tab.contexts[0]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.input.arguments == ["--arg", "error"]
    assert tc.output.exception.exception.message == "Error"


def test_parse_ctx_exception_types():
    yaml_str = """
- tab: "Ctx Exception"
  hidden: false
  testcases:
  - arguments: [ "--arg", "fail" ]
    exception:
      message: "Exception message"
      types:
        javascript: "AssertionError"
        java: "IllegalArgumentException"
  - arguments: [ "--arg", "fail2" ]
    exit_code: 10
- tab: "Ctx Error"
  testcases:
  - arguments: [ "--arg", "error" ]
    exception:
      message: "Error"
      types:
        csharp: "System.DivideByZeroException"
        python: "ZeroDivisionError"
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 2
    tab = suite.tabs[0]
    assert not tab.hidden
    assert tab.name == "Ctx Exception"
    assert len(tab.contexts) == 2
    context = tab.contexts[0]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.input.arguments == ["--arg", "fail"]
    assert tc.output.exception.exception.message == "Exception message"
    assert tc.output.exception.exception.types == {
        "javascript": "AssertionError",
        "java": "IllegalArgumentException",
    }
    context = tab.contexts[1]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.input.arguments == ["--arg", "fail2"]
    assert tc.output.exit_code.value == 10
    tab = suite.tabs[1]
    assert tab.name == "Ctx Error"
    assert len(tab.contexts) == 1
    context = tab.contexts[0]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.input.arguments == ["--arg", "error"]
    assert tc.output.exception.exception.message == "Error"
    assert tc.output.exception.exception.types == {
        "csharp": "System.DivideByZeroException",
        "python": "ZeroDivisionError",
    }


def test_parse_ctx_with_config():
    yaml_str = """
- tab: "Config ctx"
  definitions:
    stdout: &stdout
      tryFloatingPoint: true
      applyRounding: true
      roundTo: 2
    stderr: &stderr
      ignoreWhitespace: true
      caseInsensitive: false
  testcases:
  - arguments: [ '-a', '2.125', '1.212' ]
    stdout:
      data: "3.34"
      config: *stdout
  - arguments: [ '-a', '2.125', '1.212' ]
    stdout:
      data: "3.337"
      config:
        <<: *stdout
        roundTo: 3
  - arguments: [ '-a', '2.125', '1.212' ]
    stdout:
      config: *stdout
      data: "3.3"
  - arguments: [ '-e' ]
    stderr:
      config: *stderr
      data: " Fail "
    """
    args = ["-a", "2.125", "1.212"]
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert tab.hidden is None
    assert len(tab.contexts) == 4
    ctx0, ctx1, ctx2, ctx3 = tab.contexts
    assert len(ctx0.testcases) == 1
    assert len(ctx1.testcases) == 1
    assert len(ctx2.testcases) == 1
    assert len(ctx3.testcases) == 1
    tc0, tc1, tc2, tc3 = [c.testcases[0] for c in tab.contexts]
    assert tc0.input.arguments == args
    assert tc1.input.arguments == args
    assert tc2.input.arguments == args
    assert tc3.input.arguments == ["-e"]

    stdout = tc0.output.stdout
    assert stdout.data == "3.34\n"
    options = stdout.oracle.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 2

    stdout = tc1.output.stdout
    assert stdout.data == "3.337\n"
    options = stdout.oracle.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 3

    stdout = tc2.output.stdout
    assert stdout.data == "3.3\n"
    options = stdout.oracle.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 2

    stderr = tc3.output.stderr
    assert stderr.data == " Fail \n"
    options = stderr.oracle.options
    assert len(options) == 2
    assert not options["caseInsensitive"]
    assert options["ignoreWhitespace"]


def test_statements():
    yaml_str = """
- tab: "Statements"
  definitions:
    stdout: &stdout
      ignoreWhitespace: true
  contexts:
  - testcases:
    - statement: 'safe: Safe = Safe("Ignore whitespace")'
      stdout:
        data: "New safe"
        config: *stdout
    - expression: 'safe.content()'
      return: "Ignore whitespace"
  - testcases:
    - statement: 'safe: Safe = Safe(uint8(5))'
      stdout:
        data: "New safe"
        config:
          <<: *stdout
          ignoreWhitespace: false
    - expression: 'safe.content()'
      return: !expression 'uint8(5)'
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 2
    ctx0, ctx1 = tab.contexts
    tests0, tests1 = ctx0.testcases, ctx1.testcases

    assert len(tests0) == 2
    assert isinstance(tests0[0].input, VariableAssignment)
    assert tests0[0].output.stdout.data == "New safe\n"
    assert tests0[0].output.stdout.oracle.options["ignoreWhitespace"]
    assert isinstance(tests0[1].input, FunctionCall)
    assert tests0[1].output.result.value.data == "Ignore whitespace"

    assert len(tests1) == 2
    assert isinstance(tests1[0].input, VariableAssignment)
    assert tests1[0].output.stdout.data == "New safe\n"
    assert not tests1[0].output.stdout.oracle.options["ignoreWhitespace"]
    assert isinstance(tests1[1].input, FunctionCall)
    assert tests1[1].output.result.value.data == 5
    assert tests1[1].output.result.value.type == AdvancedNumericTypes.U_INT_8


def test_expression_and_main():
    yaml_str = """
- tab: "Statement and main"
  contexts:
  - testcases:
      - arguments: [ '-a', '5', '7' ]
        stdout:
          data: 12
          config:
            tryFloatingPoint: true
      - expression: 'add(5, 7)'
        return: 12
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    ctx = tab.contexts[0]
    assert len(ctx.testcases) == 2
    tc = ctx.testcases[0]
    assert tc.input.arguments == ["-a", "5", "7"]
    assert tc.output.stdout.data == "12\n"
    assert tc.output.stdout.oracle.options["tryFloatingPoint"]
    test = ctx.testcases[1]
    assert isinstance(test.input, FunctionCall)
    assert test.output.result.value.data == 12
    assert test.output.result.value.type == BasicNumericTypes.INTEGER


def test_expression():
    yaml_str = """
- tab: "Feedback"
  testcases:
  - expression: "heir(8, 10)"
    return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
  - expression: "heir(8, 3)"
    return: [ 3, 6, 9, 12, 15, 2, 7, 1, 13, 8, 16, 10, 14, 4, 11, 5 ]
"""
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 2
    ctx0, ctx1 = tab.contexts
    testcases0, testcases1 = ctx0.testcases, ctx1.testcases
    assert len(testcases0) == 1
    assert len(testcases1) == 1
    test0, test1 = testcases0[0], testcases1[0]
    assert isinstance(test0.input, FunctionCall)
    assert isinstance(test1.input, FunctionCall)


def test_expression_with_explicit_language():
    yaml_str = """
    language: "tested"
    tabs:
    - tab: "Feedback"
      testcases:
      - expression: "heir(8, 10)"
        return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
      - expression: "heir(8, 3)"
        return: [ 3, 6, 9, 12, 15, 2, 7, 1, 13, 8, 16, 10, 14, 4, 11, 5 ]
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 2
    ctx0, ctx1 = tab.contexts
    testcases0, testcases1 = ctx0.testcases, ctx1.testcases
    assert len(testcases0) == 1
    assert len(testcases1) == 1
    test0, test1 = testcases0[0], testcases1[0]
    assert isinstance(test0.input, FunctionCall)
    assert isinstance(test1.input, FunctionCall)


def test_invalid_yaml():
    yaml_str = """
- tab: "Tab"
  runs:
  - arguments: ['-a', 1, 1.2, true, no]
    stdin:
      key: value
  - arguments: ['-a', 1, 1.2, true, no]
    stderr: []
    testcases:
    - statement: 'data = () ()'
      return: !expression '() {}'
    """
    with pytest.raises(Exception):
        translate_to_test_suite(yaml_str)


def test_invalid_context_as_testcase():
    yaml_str = """
- tab: "Tab"
  contexts:
  - stdin: "5"
    statement: "5"
    return: 5
    """
    with pytest.raises(Exception):
        translate_to_test_suite(yaml_str)


def test_statement_with_yaml_dict():
    yaml_str = """
- tab: "Feedback"
  testcases:
  - expression: "get_dict()"
    return:
        alpha: 5
        beta: 6
"""
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.result.value, ObjectType)


def test_global_config_trickles_down():
    yaml_str = """
definitions:
  config: &stdout
    applyRounding: true
    roundTo: 63
    tryFloatingPoint: true
    caseInsensitive: true
namespace: "solution"
tabs:
- tab: "Ctx"
  hidden: true
  testcases:
  - arguments: [ "--arg", "argument" ]
    stdin: "Input string"
    stdout:
      data: "Output string"
      config: *stdout
    stderr: "Error string"
    exit_code: 1
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    stdout = suite.tabs[0].contexts[0].testcases[0].output.stdout
    assert isinstance(stdout.oracle, GenericTextOracle)
    config = stdout.oracle.options
    assert config["applyRounding"]
    assert config["roundTo"] == 63
    assert config["tryFloatingPoint"]
    assert config["caseInsensitive"]


def test_expression_raw_return():
    yaml_str = """
- tab: 'Test'
  contexts:
    - testcases:
        - expression: 'test()'
          return: !expression '[(4, 4), (4, 3), (4, 2), (4, 1), (4, 0), (3, 0), (3, 1), (4, 1)]'
"""
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.result.value, SequenceType)
    for element in test.output.result.value.data:
        assert element.type == AdvancedSequenceTypes.TUPLE


@pytest.mark.parametrize(
    "function_name,result",
    [
        ("set", BasicSequenceTypes.SET),
        ("sequence", BasicSequenceTypes.SEQUENCE),
        ("array", AdvancedSequenceTypes.ARRAY),
        ("tuple", AdvancedSequenceTypes.TUPLE),
        ("map", BasicObjectTypes.MAP),
    ],
)
def test_empty_constructor(function_name, result):
    yaml_str = f"""
- tab: 'Test'
  contexts:
    - testcases:
        - expression: 'test()'
          return: !expression '{function_name}()'
"""
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert test.output.result.value.type == result
    assert len(test.output.result.value.data) == 0


@pytest.mark.parametrize(
    "function_name,result",
    [
        ("set", BasicSequenceTypes.SET),
        ("sequence", BasicSequenceTypes.SEQUENCE),
        ("array", AdvancedSequenceTypes.ARRAY),
        ("tuple", AdvancedSequenceTypes.TUPLE),
    ],
)
def test_empty_constructor_with_param(function_name, result):
    yaml_str = f"""
- tab: 'Test'
  contexts:
    - testcases:
        - expression: 'test()'
          return: !expression '{function_name}([])'
"""
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert test.output.result.value.type == result
    assert len(test.output.result.value.data) == 0


def test_text_built_in_checks_implied():
    yaml_str = f"""
    - tab: 'Test'
      contexts:
        - testcases:
            - statement: 'test()'
              stdout:
                data: "hallo"
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.stdout, TextOutputChannel)
    assert isinstance(test.output.stdout.oracle, GenericTextOracle)
    assert test.output.stdout.data == "hallo\n"


def test_text_built_in_checks_explicit():
    yaml_str = f"""
    - tab: 'Test'
      contexts:
        - testcases:
            - statement: 'test()'
              stdout:
                data: "hallo"
                oracle: "builtin"
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.stdout, TextOutputChannel)
    assert isinstance(test.output.stdout.oracle, GenericTextOracle)
    assert test.output.stdout.data == "hallo\n"


def test_text_custom_checks_correct():
    yaml_str = f"""
    - tab: 'Test'
      contexts:
        - testcases:
            - statement: 'test()'
              stdout:
                data: "hallo"
                oracle: "custom_check"
                file: "test.py"
                name: "evaluate_test"
                arguments: [!expression "'yes'", 5, !expression "set([5, 5])"]
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.stdout, TextOutputChannel)
    assert isinstance(test.output.stdout.oracle, CustomCheckOracle)
    assert test.output.stdout.data == "hallo\n"
    oracle = test.output.stdout.oracle
    assert oracle.function.name == "evaluate_test"
    assert oracle.function.file == Path("test.py")
    assert oracle.arguments == [
        StringType(type=BasicStringTypes.TEXT, data="yes"),
        NumberType(type=BasicNumericTypes.INTEGER, data=5),
        SequenceType(
            type=BasicSequenceTypes.SET,
            data=[
                NumberType(type=BasicNumericTypes.INTEGER, data=5),
                NumberType(type=BasicNumericTypes.INTEGER, data=5),
            ],
        ),
    ]


def test_value_built_in_checks_implied():
    yaml_str = f"""
    - tab: 'Test'
      contexts:
        - testcases:
            - expression: 'test()'
              return: !oracle
                value: !expression "'hallo'"
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.result, ValueOutputChannel)
    assert isinstance(test.output.result.oracle, GenericValueOracle)
    assert test.output.result.value == StringType(
        type=BasicStringTypes.TEXT, data="hallo"
    )


def test_value_built_in_checks_explicit():
    yaml_str = f"""
    - tab: 'Test'
      contexts:
        - testcases:
            - expression: 'test()'
              return: !oracle
                value: "hallo"
                oracle: "builtin"
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.result, ValueOutputChannel)
    assert isinstance(test.output.result.oracle, GenericValueOracle)
    assert test.output.result.value == StringType(
        type=BasicStringTypes.TEXT, data="hallo"
    )


def test_value_custom_checks_correct():
    yaml_str = f"""
    - tab: 'Test'
      contexts:
        - testcases:
            - expression: 'test()'
              return: !oracle
                value: "hallo"
                oracle: "custom_check"
                file: "test.py"
                name: "evaluate_test"
                arguments: ['yes', 5, !expression "set([5, 5])"]
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.result, ValueOutputChannel)
    assert isinstance(test.output.result.oracle, CustomCheckOracle)
    assert test.output.result.value == StringType(
        type=BasicStringTypes.TEXT, data="hallo"
    )
    oracle = test.output.result.oracle
    assert oracle.function.name == "evaluate_test"
    assert oracle.function.file == Path("test.py")
    assert oracle.arguments == [
        StringType(type=BasicStringTypes.TEXT, data="yes"),
        NumberType(type=BasicNumericTypes.INTEGER, data=5),
        SequenceType(
            type=BasicSequenceTypes.SET,
            data=[
                NumberType(type=BasicNumericTypes.INTEGER, data=5),
                NumberType(type=BasicNumericTypes.INTEGER, data=5),
            ],
        ),
    ]


def test_yaml_set_tag_is_supported():
    yaml_str = """
- tab: 'Test'
  contexts:
    - testcases:
        - expression: 'test()'
          return: !!set {5, 6}
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 1
    testcases = tab.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.result, ValueOutputChannel)
    value = test.output.result.value
    assert isinstance(value, SequenceType)
    assert value == SequenceType(
        type=BasicSequenceTypes.SET,
        data=[
            NumberType(type=BasicNumericTypes.INTEGER, data=5),
            NumberType(type=BasicNumericTypes.INTEGER, data=6),
        ],
    )


@pytest.mark.parametrize(
    "all_types,value",
    [
        (NumericTypes, 5),
        (StringTypes, "hallo"),
        (BooleanTypes, True),
        (NothingTypes, None),
        (SequenceTypes, [5, 6]),
        (ObjectTypes, {"test": 6}),
    ],
)
def test_yaml_custom_tags_are_supported(all_types, value):
    json_type = json.dumps(value)
    for types in get_args(all_types):
        for the_type in types:
            yaml_str = f"""
        - tab: 'Test'
          contexts:
            - testcases:
                - expression: 'test()'
                  return: !{the_type} {json_type}
            """
            json_str = translate_to_test_suite(yaml_str)
            suite = parse_test_suite(json_str)
            assert len(suite.tabs) == 1
            tab = suite.tabs[0]
            assert len(tab.contexts) == 1
            testcases = tab.contexts[0].testcases
            assert len(testcases) == 1
            test = testcases[0]
            assert isinstance(test.input, FunctionCall)
            assert isinstance(test.output.result, ValueOutputChannel)
            value = test.output.result.value
            assert value.type == the_type


def test_global_language_literals():
    yaml_str = """
    language: "javascript"
    tabs:
    - tab: "Feedback"
      testcases:
      - expression: "heir(8, 10)"
        return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
      - statement:
            javascript: "hello()"
            python: "hello_2()"
"""
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 2
    ctx0, ctx1 = tab.contexts
    testcases0, testcases1 = ctx0.testcases, ctx1.testcases
    assert len(testcases0) == 1
    assert len(testcases1) == 1
    test0, test1 = testcases0[0], testcases1[0]
    assert isinstance(test0.input, LanguageLiterals)
    i1 = test0.input
    assert i1.type == "expression"
    assert i1.literals.keys() == {"javascript"}
    assert isinstance(test1.input, LanguageLiterals)
    i2 = test1.input
    assert i2.type == "statement"
    assert i2.literals.keys() == {"javascript", "python"}


def test_one_language_literal():
    yaml_str = """
    - tab: "Feedback"
      testcases:
      - expression: "heir(8, 10)"
        return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
      - statement:
            javascript: "hello()"
            python: "hello_2()"
"""
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 2
    ctx0, ctx1 = tab.contexts
    testcases0, testcases1 = ctx0.testcases, ctx1.testcases
    assert len(testcases0) == 1
    assert len(testcases1) == 1
    test0, test1 = testcases0[0], testcases1[0]
    assert isinstance(test0.input, FunctionCall)
    assert isinstance(test1.input, LanguageLiterals)
    i2 = test1.input
    assert i2.type == "statement"
    assert i2.literals.keys() == {"javascript", "python"}


@pytest.mark.parametrize(
    "old,new",
    [
        (
            """
- tab: "Ctx Exception"
  hidden: false
  testcases:
  - arguments: [ "--arg", "fail" ]
    exception:
      message: "Exception message"
      types:
        javascript: "AssertionError"
        java: "IllegalArgumentException"
  - arguments: [ "--arg", "fail2" ]
    exit_code: 10
- tab: "Ctx Error"
  testcases:
  - arguments: [ "--arg", "error" ]
    exception:
      message: "Error"
      types:
        csharp: "System.DivideByZeroException"
        python: "ZeroDivisionError"
    """,
            """
- unit: "Ctx Exception"
  hidden: false
  scripts:
  - arguments: [ "--arg", "fail" ]
    exception:
      message: "Exception message"
      types:
        javascript: "AssertionError"
        java: "IllegalArgumentException"
  - arguments: [ "--arg", "fail2" ]
    exit_code: 10
- unit: "Ctx Error"
  scripts:
  - arguments: [ "--arg", "error" ]
    exception:
      message: "Error"
      types:
        csharp: "System.DivideByZeroException"
        python: "ZeroDivisionError"
    """,
        ),
        (
            """
        language: "javascript"
        tabs:
        - tab: "Feedback"
          testcases:
          - expression: "heir(8, 10)"
            return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
          - statement:
                javascript: "hello()"
                python: "hello_2()"
    """,
            """
        language: "javascript"
        units:
        - unit: "Feedback"
          scripts:
          - expression: "heir(8, 10)"
            return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
          - statement:
                javascript: "hello()"
                python: "hello_2()"
    """,
        ),
        (
            """
    - tab: "Statement and main"
      contexts:
      - testcases:
          - arguments: [ '-a', '5', '7' ]
            stdout:
              data: 12
              config:
                tryFloatingPoint: true
          - expression: 'add(5, 7)'
            return: 12
        """,
            """
    - unit: "Statement and main"
      cases:
      - script:
          - arguments: [ '-a', '5', '7' ]
            stdout:
              data: 12
              config:
                tryFloatingPoint: true
          - expression: 'add(5, 7)'
            return: 12
        """,
        ),
    ],
)
def test_old_and_new_names_work(old, new):
    old_json = translate_to_test_suite(old)
    old_suite = parse_test_suite(old_json)
    new_json = translate_to_test_suite(new)
    new_suite = parse_test_suite(new_json)

    assert old_suite == new_suite


def test_additional_properties_are_not_allowed():
    yaml_str = """
    - tab: "Feedback"
      testcases:
      - expression: "heir(8, 10)"
        not_return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
      - statement:
            javascript: "hello()"
            python: "hello_2()"
"""
    with pytest.raises(Exception):
        parse_dsl(yaml_str)


def test_files_are_propagated():
    yaml_str = """
- tab: "Config ctx"
  files:
    - name: "test"
      url: "test.md"
    - name: "two"
      url: "two.md"
  testcases:
  - arguments: [ '-a', '2.125', '1.212' ]
    stdout: "3.34"
  - arguments: [ '-a', '2.125', '1.212' ]
    stdout: "3.337"
    files:
        - name: "test"
          url: "twooo.md"
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    tab = suite.tabs[0]
    ctx0, ctx1 = tab.contexts
    testcases0, testcases1 = ctx0.testcases, ctx1.testcases
    test0, test1 = testcases0[0], testcases1[0]
    assert set(test0.link_files) == {
        FileUrl(name="test", url="test.md"),
        FileUrl(name="two", url="two.md"),
    }


def test_newlines_are_added_to_stdout():
    yaml_str = """
- unit: "Statement and main"
  cases:
  - script:
      - arguments: [ '-a', '5', '7' ]
        stdout:
          data: 12
          config:
            tryFloatingPoint: true
        """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    actual_stdout = suite.tabs[0].contexts[0].testcases[0].output.stdout.data
    assert actual_stdout == "12\n"

    yaml_str2 = """
- unit: "Statement and main"
  cases:
  - script:
      - arguments: [ '-a', '5', '7' ]
        stdout: "hello"
        """
    json_str = translate_to_test_suite(yaml_str2)
    suite = parse_test_suite(json_str)
    actual_stdout = suite.tabs[0].contexts[0].testcases[0].output.stdout.data
    assert actual_stdout == "hello\n"


def test_newlines_are_added_to_stderr():
    yaml_str = """
- unit: "Statement and main"
  cases:
  - script:
      - arguments: [ '-a', '5', '7' ]
        stderr:
          data: 12
          config:
            tryFloatingPoint: true
        """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    actual_stderr = suite.tabs[0].contexts[0].testcases[0].output.stderr.data
    assert actual_stderr == "12\n"

    yaml_str2 = """
- unit: "Statement and main"
  cases:
  - script:
      - arguments: [ '-a', '5', '7' ]
        stderr: "hello"
        """
    json_str = translate_to_test_suite(yaml_str2)
    suite = parse_test_suite(json_str)
    actual_stderr = suite.tabs[0].contexts[0].testcases[0].output.stderr.data
    assert actual_stderr == "hello\n"


def test_no_duplicate_newlines_are_added():
    yaml_str = """
- unit: "Statement and main"
  cases:
  - script:
      - arguments: [ '-a', '5', '7' ]
        stdout: |
            hello
            world
        """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    actual = suite.tabs[0].contexts[0].testcases[0].output.stdout.data
    assert actual == "hello\nworld\n"


def test_can_disable_normalizing_newlines():
    yaml_str = """
- unit: "Statement and main"
  cases:
  - script:
      - arguments: [ '-a', '5', '7' ]
        stderr:
          data: 12
          config:
            tryFloatingPoint: true
            normalizeTrailingNewlines: false
        """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    actual_stderr = suite.tabs[0].contexts[0].testcases[0].output.stderr.data
    assert actual_stderr == "12"


def test_empty_text_data_newlines():
    yaml_str = """
- unit: "Statement and main"
  cases:
  - script:
      - arguments: [ '-a', '5', '7' ]
        stderr: ""
        """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    actual_stderr = suite.tabs[0].contexts[0].testcases[0].output.stderr.data
    assert actual_stderr == ""
