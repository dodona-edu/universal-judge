import pytest

from tested.datatypes import AdvancedNumericTypes, BasicNumericTypes
from tested.dsl import translate_to_test_suite
from tested.serialisation import Assignment, FunctionCall, ObjectType
from tested.testsuite import parse_test_suite


def test_parse_one_tab_ctx():
    yaml_str = """
disableOptimizations: true
namespace: "solution"
tabs:
- tab: "Ctx"
  hidden: true
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
    assert tab.hidden
    assert tab.name == "Ctx"
    assert len(tab.contexts) == 1
    context = tab.contexts[0]
    assert len(context.testcases) == 1
    tc = context.testcases[0]
    assert tc.is_main_testcase()
    assert tc.input.stdin.data == "Input string"
    assert tc.input.arguments == ["--arg", "argument"]
    assert tc.output.stderr.data == "Error string"
    assert tc.output.stdout.data == "Output string"
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


def test_parse_ctx_with_config():
    yaml_str = """
- tab: "Config ctx"
  config:
    stdout:
      tryFloatingPoint: true
      applyRounding: true
      roundTo: 2
    stderr:
      ignoreWhitespace: true
      caseInsensitive: false
  testcases:
  - arguments: [ '-a', 2.125, 1.212 ]
    stdout: "3.34"
  - arguments: [ '-a', 2.125, 1.212 ]
    stdout:
      data: "3.337"
      config:
        roundTo: 3
  - config:
      stdout:
        roundTo: 1
    arguments: [ '-a', 2.125, 1.212 ]
    stdout: "3.3"
  - arguments: [ '-e' ]
    stderr: " Fail "
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
    assert stdout.data == "3.34"
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 2

    stdout = tc1.output.stdout
    assert stdout.data == "3.337"
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 3

    stdout = tc2.output.stdout
    assert stdout.data == "3.3"
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 1

    stderr = tc3.output.stderr
    assert stderr.data == " Fail "
    options = stderr.evaluator.options
    assert len(options) == 2
    assert not options["caseInsensitive"]
    assert options["ignoreWhitespace"]


def test_statements():
    yaml_str = """
- tab: "Statements"
  config:
    stdout:
      ignoreWhitespace: true
  contexts:
  - testcases:
    - statement: 'safe: Safe = Safe("Ignore whitespace")'
      stdout: "New safe"
    - expression: 'safe.content()'
      return: "Ignore whitespace"
  - testcases:
    - statement: 'safe: Safe = Safe(uint8(5))'
      stdout:
        data: "New safe"
        config:
          ignoreWhitespace: false
    - expression: 'safe.content()'
      return_raw: 'uint8(5)'
    """
    json_str = translate_to_test_suite(yaml_str)
    suite = parse_test_suite(json_str)
    assert len(suite.tabs) == 1
    tab = suite.tabs[0]
    assert len(tab.contexts) == 2
    ctx0, ctx1 = tab.contexts
    tests0, tests1 = ctx0.testcases, ctx1.testcases

    assert len(tests0) == 2
    assert isinstance(tests0[0].input, Assignment)
    assert tests0[0].output.stdout.data == "New safe"
    assert tests0[0].output.stdout.evaluator.options["ignoreWhitespace"]
    assert isinstance(tests0[1].input, FunctionCall)
    assert tests0[1].output.result.value.data == "Ignore whitespace"

    assert len(tests1) == 2
    assert isinstance(tests1[0].input, Assignment)
    assert tests1[0].output.stdout.data == "New safe"
    assert not tests1[0].output.stdout.evaluator.options["ignoreWhitespace"]
    assert isinstance(tests1[1].input, FunctionCall)
    assert tests1[1].output.result.value.data == 5
    assert tests1[1].output.result.value.type == AdvancedNumericTypes.U_INT_8


def test_statement_and_main():
    yaml_str = """
- tab: "Statement and main"
  contexts:
  - testcases:
      - arguments: [ '-a', 5, 7 ]
        stdout:
          data: 12
          config:
            tryFloatingPoint: true
      - statement: 'add(5, 7)'
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
    assert tc.input.main_call
    assert tc.input.arguments == ["-a", "5", "7"]
    assert tc.output.stdout.data == "12"
    assert tc.output.stdout.evaluator.options["tryFloatingPoint"]
    test = ctx.testcases[1]
    assert isinstance(test.input, FunctionCall)
    assert test.output.result.value.data == 12
    assert test.output.result.value.type == BasicNumericTypes.INTEGER


def test_statement():
    yaml_str = """
- tab: "Feedback"
  testcases:
  - expression: "heir(8, 10)"
    return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
  - statement: "heir(8, 3)"
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
      return_raw: '() {}'
    """
    with pytest.raises(ValueError):
        translate_to_test_suite(yaml_str)


def test_invalid_mutual_exclusive_return_yaml():
    yaml_str = """
- tab: "Tab"
  contexts:
  - testcases:
    - statement: "5"
      return: 5
      return_raw: "5"
    """
    with pytest.raises(ValueError):
        translate_to_test_suite(yaml_str)


def test_invalid_context_as_testcase():
    yaml_str = """
- tab: "Tab"
  contexts:
  - stdin: "5"
    statement: "5"
    return: 5
    """
    with pytest.raises(ValueError):
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
