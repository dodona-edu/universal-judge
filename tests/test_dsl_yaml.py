from tested.datatypes import AdvancedNumericTypes, BasicNumericTypes
from tested.dsl import translate
from tested.serialisation import Assignment, FunctionCall
from tested.testplan import _PlanModel


def test_parse_one_tab_ctx():
    yaml_str = """
- tab: "Ctx"
  runs:
  - arguments: [ "--arg", "argument" ]
    stdin: "Input string"
    stdout: "Output string"
    stderr: "Error string"
    exit_code: 1
    """
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert tab.name == "Ctx"
    assert len(tab.contexts) == 1
    context = tab.contexts[0]
    assert len(context.testcases) == 0
    ctx_input = context.context_testcase.input
    assert ctx_input.stdin.data == "Input string"
    assert ctx_input.arguments == ["--arg", "argument"]
    assert ctx_input.main_call
    ctx_output = context.context_testcase.output
    assert ctx_output.stderr.data == "Error string"
    assert ctx_output.stdout.data == "Output string"
    assert ctx_output.exit_code.value == 1


def test_parse_ctx_exception():
    yaml_str = """
- tab: "Ctx Exception"
  runs:
  - arguments: [ "--arg", "fail" ]
    exception: "Exception message"
  - arguments: [ "--arg", "fail2" ]
    exit_code: 10
- tab: "Ctx Error"
  runs:
  - arguments: [ "--arg", "error" ]
    exception: "Error"
    """
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 2
    tab = plan.tabs[0]
    assert tab.name == "Ctx Exception"
    assert len(tab.contexts) == 2
    context = tab.contexts[0]
    assert len(context.testcases) == 0
    assert context.context_testcase.input.arguments == ["--arg", "fail"]
    assert context.context_testcase.output.exception.exception.message == "Exception message"
    context = tab.contexts[1]
    assert len(context.testcases) == 0
    assert context.context_testcase.input.arguments == ["--arg", "fail2"]
    assert context.context_testcase.output.exit_code.value == 10
    tab = plan.tabs[1]
    assert tab.name == "Ctx Error"
    assert len(tab.contexts) == 1
    context = tab.contexts[0]
    assert len(context.testcases) == 0
    assert context.context_testcase.input.arguments == ["--arg", "error"]
    assert context.context_testcase.output.exception.exception.message == "Error"


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
  runs:
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
    assert len(tab.contexts) == 4
    ctx0, ctx1, ctx2, ctx3 = tab.contexts
    # Check argument list
    assert ctx0.context_testcase.input.arguments == args
    assert ctx1.context_testcase.input.arguments == args
    assert ctx2.context_testcase.input.arguments == args
    assert ctx3.context_testcase.input.arguments == ['-e']

    stdout = ctx0.context_testcase.output.stdout
    assert stdout.data == '3.34'
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 2

    stdout = ctx1.context_testcase.output.stdout
    assert stdout.data == '3.337'
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 3

    stdout = ctx2.context_testcase.output.stdout
    assert stdout.data == '3.3'
    options = stdout.evaluator.options
    assert len(options) == 3
    assert options["tryFloatingPoint"]
    assert options["applyRounding"]
    assert options["roundTo"] == 1

    stderr = ctx3.context_testcase.output.stderr
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
  runs:
  - testcases:
    - statement: 'Safe safe = new Safe("Ignore whitespace")'
      stdout: "New safe"
    - statement: 'safe.content()'
      return: "Ignore whitespace"
  - testcases:
    - statement: 'Safe safe = new Safe(5 :: uint8)'
      stdout:
        data: "New safe"
        config:
          ignoreWhitespace: false
    - statement: 'safe.content()'
      return-raw: '5 :: uint8'
    """
    json_str = translate(yaml_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert len(tab.contexts) == 2
    ctx0, ctx1 = tab.contexts
    assert not ctx0.context_testcase.input.main_call
    assert not ctx1.context_testcase.input.main_call
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
  runs:
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
    print(json_str)
    plan = _PlanModel.parse_raw(json_str).__root__
    assert len(plan.tabs) == 1
    tab = plan.tabs[0]
    assert len(tab.contexts) == 1
    ctx = tab.contexts[0]
    assert ctx.context_testcase.input.main_call
    assert ctx.context_testcase.input.arguments == ['-a', '5', '7']
    assert ctx.context_testcase.output.stdout.data == '12'
    assert ctx.context_testcase.output.stdout.evaluator.options['tryFloatingPoint']
    assert len(ctx.testcases) == 1
    test = ctx.testcases[0]
    assert isinstance(test.input, FunctionCall)
    assert test.output.result.value.data == 12
    assert test.output.result.value.type == BasicNumericTypes.INTEGER


def test_invalid_yaml(caplog):
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
    assert translate(yaml_str) is None
    assert len(caplog.record_tuples) == 8
