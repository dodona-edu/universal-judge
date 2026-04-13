import sys
from pathlib import Path

import pytest

from tested.configs import create_bundle
from tested.judge.planning import (
    PlannedContext,
    PlannedExecutionUnit,
    PlanStrategy,
    plan_test_suite,
)
from tested.testsuite import (
    ContentPath,
    Context,
    EmptyChannel,
    ExitCodeOutputChannel,
    FileOutputChannel,
    LanguageLiterals,
    MainInput,
    Output,
    Suite,
    SupportedLanguage,
    Tab,
)
from tested.testsuite import Testcase as SuiteTestcase
from tested.testsuite import TextData
from tests.manual_utils import configuration


def test_planned_execution_unit_get_stdin():
    resources = Path("resources")
    tc1 = SuiteTestcase(
        input=MainInput(stdin=TextData(content="stdin1")), input_files=[]
    )
    tc2 = SuiteTestcase(
        input=MainInput(stdin=TextData(content="stdin2")), input_files=[]
    )
    tc3 = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])

    pc1 = PlannedContext(context=Context(testcases=[tc1]), tab_index=0, context_index=0)
    pc2 = PlannedContext(context=Context(testcases=[tc2]), tab_index=0, context_index=1)
    pc3 = PlannedContext(context=Context(testcases=[tc3]), tab_index=0, context_index=2)

    unit = PlannedExecutionUnit(contexts=[pc1, pc2, pc3], name="unit", index=0)
    # tc1 has stdin1, tc2 has stdin2, tc3 has no stdin.
    assert unit.get_stdin(resources) == "stdin1stdin2"


def test_planned_execution_unit_has_main_testcase():
    tc_main = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    tc_not_main = SuiteTestcase(
        input=LanguageLiterals(literals={SupportedLanguage.PYTHON: "5"}), input_files=[]
    )

    pc_main = PlannedContext(
        context=Context(testcases=[tc_main]), tab_index=0, context_index=0
    )
    pc_not_main = PlannedContext(
        context=Context(testcases=[tc_not_main]), tab_index=0, context_index=1
    )

    unit_with_main = PlannedExecutionUnit(contexts=[pc_main], name="unit1", index=0)
    assert unit_with_main.has_main_testcase() is True

    unit_without_main = PlannedExecutionUnit(
        contexts=[pc_not_main], name="unit2", index=1
    )
    assert unit_without_main.has_main_testcase() is False


def test_planned_execution_unit_has_exit_testcase():
    tc_exit = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(exit_code=ExitCodeOutputChannel(value=0)),
        input_files=[],
    )
    tc_no_exit = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])

    pc_exit = PlannedContext(
        context=Context(testcases=[tc_exit]), tab_index=0, context_index=0
    )
    pc_no_exit = PlannedContext(
        context=Context(testcases=[tc_no_exit]), tab_index=0, context_index=1
    )

    unit_with_exit = PlannedExecutionUnit(contexts=[pc_exit], name="unit1", index=0)
    assert unit_with_exit.has_exit_testcase() is True

    unit_without_exit = PlannedExecutionUnit(
        contexts=[pc_no_exit], name="unit2", index=1
    )
    assert unit_without_exit.has_exit_testcase() is False


def test_plan_strategy_optimal(tmp_path: Path, pytestconfig: pytest.Config):
    tc = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    ctx1 = Context(testcases=[tc])
    ctx2 = Context(testcases=[tc])
    tab1 = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab1])

    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 1
    assert len(units[0].contexts) == 2
    assert units[0].name == "execution_0"


def test_plan_strategy_optimal_multiple_tabs(
    tmp_path: Path, pytestconfig: pytest.Config
):
    tc = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    ctx1 = Context(testcases=[tc])
    ctx2 = Context(testcases=[tc])
    tab1 = Tab(name="tab1", contexts=[ctx1])
    tab2 = Tab(name="tab2", contexts=[ctx2])
    suite = Suite(tabs=[tab1, tab2])

    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    # Should be 1 unit because there are no conflicts
    assert len(units) == 1
    assert len(units[0].contexts) == 2
    assert units[0].contexts[0].tab_index == 0
    assert units[0].contexts[1].tab_index == 1


def test_plan_strategy_tab(tmp_path: Path, pytestconfig: pytest.Config):
    tc = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    ctx1 = Context(testcases=[tc])
    ctx2 = Context(testcases=[tc])
    tab1 = Tab(name="tab1", contexts=[ctx1])
    tab2 = Tab(name="tab2", contexts=[ctx2])
    suite = Suite(tabs=[tab1, tab2])

    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.TAB)

    assert len(units) == 2
    assert len(units[0].contexts) == 1
    assert len(units[1].contexts) == 1
    assert units[0].name == "execution_0"
    assert units[1].name == "execution_1"


def test_plan_strategy_context(tmp_path: Path, pytestconfig: pytest.Config):
    tc = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    ctx1 = Context(testcases=[tc])
    ctx2 = Context(testcases=[tc])
    tab1 = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab1])

    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.CONTEXT)

    assert len(units) == 2
    assert len(units[0].contexts) == 1
    assert len(units[1].contexts) == 1


def test_planning_conflict_input_files(tmp_path: Path, pytestconfig: pytest.Config):
    # Same path, different content
    file1 = TextData(path="file.txt", content="content1")
    file2 = TextData(path="file.txt", content="content2")

    ctx1 = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1])
        ]
    )
    ctx2 = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file2])
        ]
    )

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_no_conflict_input_files(tmp_path: Path, pytestconfig: pytest.Config):
    # Same path, same content
    file1 = TextData(path="file.txt", content="content1")
    file2 = TextData(path="file.txt", content="content1")

    ctx1 = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1])
        ]
    )
    ctx2 = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file2])
        ]
    )

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 1


def test_planning_conflict_output_files(tmp_path: Path, pytestconfig: pytest.Config):
    # Same output path
    file1 = TextData(path="out.txt", content="content1")
    file2 = TextData(path="out.txt", content="content2")

    tc1 = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(file=FileOutputChannel(files=[file1])),
    )
    tc2 = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(file=FileOutputChannel(files=[file2])),
    )

    ctx1 = Context(testcases=[tc1])
    ctx2 = Context(testcases=[tc2])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_conflict_stdin(tmp_path: Path, pytestconfig: pytest.Config):
    # Context with stdin should start a new unit
    tc1 = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    tc2 = SuiteTestcase(
        input=MainInput(stdin=TextData(content="stdin")), input_files=[]
    )

    ctx1 = Context(testcases=[tc1])
    ctx2 = Context(testcases=[tc2])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2
    assert len(units[0].contexts) == 1
    assert len(units[1].contexts) == 1
    assert units[1].contexts[0].context == ctx2


def test_planning_conflict_exit_code(tmp_path: Path, pytestconfig: pytest.Config):
    # Context with exit code check should end the unit
    tc1 = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(exit_code=ExitCodeOutputChannel(value=0)),
    )
    tc2 = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE))

    ctx1 = Context(testcases=[tc1])
    ctx2 = Context(testcases=[tc2])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2
    assert len(units[0].contexts) == 1
    assert len(units[1].contexts) == 1
    assert units[0].contexts[0].context == ctx1


def test_plan_empty_suite(tmp_path: Path, pytestconfig: pytest.Config):
    suite = Suite(tabs=[])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)

    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)
    assert len(units) == 0


def test_planning_stdin_no_split_after(tmp_path: Path, pytestconfig: pytest.Config):
    # [Stdin, NoStdin] -> 1 unit (no split because the second one has no stdin)
    tc1 = SuiteTestcase(input=MainInput(stdin=TextData(content="stdin")))
    tc2 = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE))

    ctx1 = Context(testcases=[tc1])
    ctx2 = Context(testcases=[tc2])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 1
    assert len(units[0].contexts) == 2


def test_planning_exit_code_no_split_before(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # [NoExitCode, ExitCode] -> 1 unit (no split because the first one has no exit code, and the second one ends the unit)
    tc1 = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE))
    tc2 = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(exit_code=ExitCodeOutputChannel(value=0)),
    )

    ctx1 = Context(testcases=[tc1])
    ctx2 = Context(testcases=[tc2])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 1
    assert len(units[0].contexts) == 2


def test_planning_multiple_input_files_conflict(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Context with multiple input files, one of which conflicts
    file1 = TextData(path="file1.txt", content="content1")
    file2 = TextData(path="file2.txt", content="content2")
    file1_alt = TextData(path="file1.txt", content="content1_alt")

    ctx1 = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1, file2]
            )
        ]
    )
    ctx2 = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1_alt]
            )
        ]
    )

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_multiple_output_files_conflict(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Context with multiple output files, one of which conflicts
    out1 = TextData(path="out1.txt", content="content1")
    out2 = TextData(path="out2.txt", content="content2")
    out1_alt = TextData(path="out1.txt", content="content1_alt")

    tc1 = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(file=FileOutputChannel(files=[out1, out2])),
    )
    tc2 = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(file=FileOutputChannel(files=[out1_alt])),
    )

    ctx1 = Context(testcases=[tc1])
    ctx2 = Context(testcases=[tc2])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_state_cleared_after_split(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Verify that file tracking is cleared after a split
    file1 = TextData(path="file1.txt", content="content1")
    file1_alt = TextData(path="file1.txt", content="content1_alt")

    # ctx1 and ctx2 will split because of stdin
    ctx1 = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1])
        ]
    )
    ctx2 = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=TextData(content="stdin")),
                input_files=[file1_alt],
            )
        ]
    )
    ctx3 = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1])
        ]
    )

    tab = Tab(name="tab1", contexts=[ctx1, ctx2, ctx3])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 3
    assert len(units[0].contexts) == 1
    assert len(units[1].contexts) == 1
    assert len(units[2].contexts) == 1


def test_planning_mixed_conflicts(tmp_path: Path, pytestconfig: pytest.Config):
    file1 = TextData(path="file1.txt", content="content1")
    file1_alt = TextData(path="file1.txt", content="content2")
    out2 = TextData(path="file2.txt", content="out")

    ctx1 = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1])
        ]
    )
    ctx2 = Context(
        testcases=[SuiteTestcase(input=MainInput(stdin=TextData(content="stdin")))]
    )
    ctx3 = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE),
                output=Output(file=FileOutputChannel(files=[out2])),
            )
        ]
    )
    ctx4 = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1_alt]
            )
        ]
    )
    ctx5 = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE),
                output=Output(exit_code=ExitCodeOutputChannel(value=0)),
            )
        ]
    )
    ctx6 = Context(testcases=[SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE))])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2, ctx3, ctx4, ctx5, ctx6])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 5
    assert [len(u.contexts) for u in units] == [1, 2, 1, 1, 1]
    assert units[0].contexts[0].context == ctx1
    assert units[1].contexts[0].context == ctx2
    assert units[1].contexts[1].context == ctx3
    assert units[2].contexts[0].context == ctx4
    assert units[3].contexts[0].context == ctx5
    assert units[4].contexts[0].context == ctx6


def test_planning_sequential_exit_codes(tmp_path: Path, pytestconfig: pytest.Config):
    # [ExitCode, ExitCode] -> 2 units
    tc = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        output=Output(exit_code=ExitCodeOutputChannel(value=0)),
    )
    ctx1 = Context(testcases=[tc])
    ctx2 = Context(testcases=[tc])

    tab = Tab(name="tab1", contexts=[ctx1, ctx2])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2
    assert len(units[0].contexts) == 1
    assert len(units[1].contexts) == 1


def test_planning_conflict_input_file_forward_leak(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # ctx A has a dynamic file; ctx B has no input files.
    # The file written for A would persist into B, so they must be split.
    file_a = TextData(path="file.txt", content="content")
    ctx_a = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE), input_files=[file_a]
            )
        ]
    )
    ctx_b = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
        ]
    )

    tab = Tab(name="tab1", contexts=[ctx_a, ctx_b])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_conflict_input_file_backward_leak(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # ctx A has no input files; ctx B has a dynamic file.
    # Files are written upfront, so B's file would already be present during A.
    ctx_a = Context(
        testcases=[
            SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
        ]
    )
    file_b = TextData(path="file.txt", content="content")
    ctx_b = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE), input_files=[file_b]
            )
        ]
    )

    tab = Tab(name="tab1", contexts=[ctx_a, ctx_b])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_no_conflict_static_input_files_lax(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Both contexts use the same static ContentPath file in lax mode.
    # Static files are already in the workdir, so no conflict arises.
    static_file = TextData(path="f.txt", content=ContentPath(path="f.txt"))
    ctx_a = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE), input_files=[static_file]
            )
        ]
    )
    ctx_b = Context(
        testcases=[
            SuiteTestcase(
                input=MainInput(stdin=EmptyChannel.NONE), input_files=[static_file]
            )
        ]
    )

    tab = Tab(name="tab1", contexts=[ctx_a, ctx_b])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 1


def test_planning_conflict_strict_then_lax(tmp_path: Path, pytestconfig: pytest.Config):
    # ctx A is strict (declares input_files); ctx B is lax (no input_files).
    # Strict ctx includes static files in its footprint, so the sets differ.
    strict_file = TextData(path="f.txt", content=ContentPath(path="f.txt"))
    tc_a = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[strict_file],
        use_strict_workdir=True,
    )
    ctx_a = Context(testcases=[tc_a])
    tc_b = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    ctx_b = Context(testcases=[tc_b])

    tab = Tab(name="tab1", contexts=[ctx_a, ctx_b])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_conflict_lax_then_strict(tmp_path: Path, pytestconfig: pytest.Config):
    # ctx A is lax; ctx B is strict. B's static file footprint differs from A's empty set.
    tc_a = SuiteTestcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    ctx_a = Context(testcases=[tc_a])
    strict_file = TextData(path="f.txt", content=ContentPath(path="f.txt"))
    tc_b = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[strict_file],
        use_strict_workdir=True,
    )
    ctx_b = Context(testcases=[tc_b])

    tab = Tab(name="tab1", contexts=[ctx_a, ctx_b])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2


def test_planning_no_conflict_strict_same_files(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Both contexts are strict with the same ContentPath file → 1 unit.
    strict_file = TextData(path="f.txt", content=ContentPath(path="f.txt"))
    tc = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[strict_file],
        use_strict_workdir=True,
    )
    ctx_a = Context(testcases=[tc])
    ctx_b = Context(testcases=[tc])

    tab = Tab(name="tab1", contexts=[ctx_a, ctx_b])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 1


def test_planning_conflict_strict_different_files(
    tmp_path: Path, pytestconfig: pytest.Config
):
    # Both contexts are strict but declare different files → 2 units.
    file_a = TextData(path="a.txt", content=ContentPath(path="a.txt"))
    tc_a = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[file_a],
        use_strict_workdir=True,
    )
    ctx_a = Context(testcases=[tc_a])

    file_b = TextData(path="b.txt", content=ContentPath(path="b.txt"))
    tc_b = SuiteTestcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[file_b],
        use_strict_workdir=True,
    )
    ctx_b = Context(testcases=[tc_b])

    tab = Tab(name="tab1", contexts=[ctx_a, ctx_b])
    suite = Suite(tabs=[tab])
    conf = configuration(pytestconfig, "echo-function", "python", tmp_path)
    bundle = create_bundle(conf, sys.stdout, suite)
    units = plan_test_suite(bundle, PlanStrategy.OPTIMAL)

    assert len(units) == 2
