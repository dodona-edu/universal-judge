import pytest

from tested.datatypes import AdvancedNumericTypes, BasicNumericTypes
from tested.dsl import SchemaParser, ParseError
from tested.serialisation import Assignment, FunctionCall, ObjectType
from tested.testplan import _PlanModel

parser = SchemaParser()
translate = parser.translate_str


def test_parse_one_tab_ctx():
    yaml_str = """
disable_optimizations: true
namespace: "solution"
tabs:
- tab: "Ctx"
  hidden: true
  contexts:
  - arguments: [ "--arg", "argument" ]
    stdin: "Input string"
    stdout: "Output string"
    stderr: "Error string"
    exit_code: 1
    """
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert plan.namespace == "solution"
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert tab.hidden
    assert tab.name == "Ctx"
    assert len(tab.runs) == 1
    run = tab.runs[0]
    assert len(run.contexts) == 0
    run_input = run.run.input
    assert run_input.stdin.data == "Input string"
    assert run_input.arguments == ["--arg", "argument"]
    assert run_input.main_call
    run_output = run.run.output
    assert run_output.stderr.data == "Error string"
    assert run_output.stdout.data == "Output string"
    assert run_output.exit_code.value == 1


def test_parse_ctx_exception():
    yaml_str = """
- tab: "Ctx Exception"
  hidden: false
  contexts:
  - arguments: [ "--arg", "fail" ]
    exception: "Exception message"
  - arguments: [ "--arg", "fail2" ]
    exit_code: 10
- tab: "Ctx Error"
  contexts:
  - arguments: [ "--arg", "error" ]
    exception: "Error"
    """
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 2
    tab = plan.tabs[0]
    assert not tab.hidden
    assert tab.name == "Ctx Exception"
    assert len(tab.runs) == 2
    run = tab.runs[0]
    assert len(run.contexts) == 0
    assert run.run.input.arguments == ["--arg", "fail"]
    assert run.run.output.exception.exception.message == "Exception message"
    run = tab.runs[1]
    assert len(run.contexts) == 0
    assert run.run.input.arguments == ["--arg", "fail2"]
    assert run.run.output.exit_code.value == 10
    tab = plan.tabs[1]
    assert tab.name == "Ctx Error"
    assert len(tab.runs) == 1
    run = tab.runs[0]
    assert len(run.contexts) == 0
    assert run.run.input.arguments == ["--arg", "error"]
    assert run.run.output.exception.exception.message == "Error"


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
  contexts:
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
    args = ['-a', "2.125", "1.212"]
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert tab.hidden is None
    assert len(tab.runs) == 4
    ctx0, ctx1, ctx2, ctx3 = tab.runs
    # Check argument list
    assert ctx0.run.input.arguments == args
    assert ctx1.run.input.arguments == args
    assert ctx2.run.input.arguments == args
    assert ctx3.run.input.arguments == ['-e']

    stdout = ctx0.run.output.stdout
    assert stdout.data == '3.34'
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 2

    stdout = ctx1.run.output.stdout
    assert stdout.data == '3.337'
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 3

    stdout = ctx2.run.output.stdout
    assert stdout.data == '3.3'
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 1

    stderr = ctx3.run.output.stderr
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
    - statement: 'Safe safe = new Safe("Ignore whitespace")'
      stdout: "New safe"
    - expression: 'safe.content()'
      return: "Ignore whitespace"
  - testcases:
    - statement: 'Safe safe = new Safe(5 :: uint8)'
      stdout:
        data: "New safe"
        config:
          ignoreWhitespace: false
    - expression: 'safe.content()'
      return-raw: '5 :: uint8'
    """
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert len(tab.runs) == 1
    run = tab.runs[0]
    assert not run.run.input.main_call
    ctx0, ctx1 = run.contexts
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
  - arguments: [ '-a', 5, 7 ]
    stdout:
      data: 12
      config:
        tryFloatingPoint: true
    testcases:
      - statement: 'add(5, 7)'
        return: 12
    """
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert len(tab.runs) == 1
    run = tab.runs[0]
    assert run.run.input.main_call
    assert run.run.input.arguments == ['-a', '5', '7']
    assert run.run.output.stdout.data == '12'
    assert run.run.output.stdout.evaluator.options['tryFloatingPoint']
    assert len(run.contexts) == 1
    ctx = run.contexts[0]
    assert len(ctx.testcases) == 1
    test = ctx.testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert test.output.result.value.data == 12
    assert test.output.result.value.type == BasicNumericTypes.INTEGER


def test_statement():
    yaml_str = """
- tab: "Feedback"
  contexts:
  - expression: "heir(8, 10)"
    return: [ 10, 4, 15, 11, 7, 5, 3, 2, 16, 12, 1, 6, 13, 9, 14, 8 ]
  - statement: "heir(8, 3)"
    return: [ 3, 6, 9, 12, 15, 2, 7, 1, 13, 8, 16, 10, 14, 4, 11, 5 ]
"""
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert len(tab.runs) == 1
    run = tab.runs[0]
    assert not run.run.input.main_call
    ctx0, ctx1 = run.contexts
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
      return-raw: '() {}'
    """
    with pytest.raises(SystemExit) as e:
        translate(yaml_str)
    assert e.type == SystemExit
    assert e.value.code != 0


def test_invalid_mutual_exclusive_return_yaml():
    yaml_str = """
- tab: "Tab"
  contexts:
  - testcases:
    - statement: "5"
      return: 5
      return-raw: "5"
    """
    with pytest.raises(SystemExit) as e:
        translate(yaml_str)
    assert e.type == SystemExit
    assert e.value.code != 0


def test_invalid_mutual_exclusive_context_testcase():
    yaml_str = """
- tab: "Tab"
  contexts:
  - stdin: "5"
    statement: "5"
    return: 5
    """
    with pytest.raises(SystemExit) as e:
        translate(yaml_str)
    assert e.type == SystemExit
    assert e.value.code != 0


def test_statement_with_yaml_dict():
    yaml_str = """
- tab: "Feedback"
  contexts:
  - expression: "get_dict()"
    return:
        alpha: 5
        beta: 6
"""
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert len(tab.runs) == 1
    run = tab.runs[0]
    assert not run.run.input.main_call
    testcases = run.contexts[0].testcases
    assert len(testcases) == 1
    test = testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert isinstance(test.output.result.value, ObjectType)

    pass
