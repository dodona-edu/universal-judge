from io import StringIO
from pathlib import Path

import pytest

from tested.configs import create_bundle
from tested.dodona import (
    AnnotateCode,
    CloseContext,
    CloseTab,
    CloseTest,
    CloseTestcase,
    StartContext,
    StartJudgement,
    StartTab,
    StartTest,
    StartTestcase,
    Status,
    StatusMessage,
)
from tested.judge.collector import OutputManager
from tested.judge.core import judge
from tested.judge.evaluation import terminate
from tested.serialisation import FunctionCall, FunctionType
from tested.testsuite import (
    Context,
    FileOutputChannel,
    MainInput,
    Output,
    Suite,
    SupportedLanguage,
    Tab,
    Testcase,
    TextData,
)
from tests.manual_utils import assert_valid_output, configuration

TEST_SUITE = Suite(
    tabs=[
        Tab(
            name="Tab 1",
            contexts=[
                Context(
                    testcases=[
                        Testcase(
                            input=MainInput(arguments=["hello 1"]), output=Output()
                        ),
                        Testcase(
                            input=FunctionCall(
                                type=FunctionType.FUNCTION, name="test 2", arguments=[]
                            ),
                            output=Output(),
                        ),
                    ]
                ),
                Context(
                    testcases=[
                        Testcase(
                            input=FunctionCall(
                                type=FunctionType.FUNCTION, name="test 22", arguments=[]
                            ),
                            output=Output(),
                        ),
                    ]
                ),
            ],
        ),
        Tab(
            name="Tab 2",
            contexts=[
                Context(
                    testcases=[
                        Testcase(
                            input=FunctionCall(
                                type=FunctionType.FUNCTION,
                                name="test 2.1",
                                arguments=[],
                            ),
                            output=Output(),
                        )
                    ]
                )
            ],
        ),
    ]
)


def test_mid_judgement_is_completed(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, TEST_SUITE)
    collector = OutputManager(out=result)

    # Only the first tab has been executed.
    collector.add(StartJudgement())
    collector.add(StartTab(TEST_SUITE.tabs[0].name))
    collector.add(StartContext())
    collector.add(StartTestcase(description="test 1"))
    collector.add(StartTest(expected="test 1.1"))
    collector.add(
        CloseTest(generated="test 1.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 0)
    collector.add(StartTestcase(description="test 2"))
    collector.add(StartTest(expected="test 2.1"))
    collector.add(
        CloseTest(generated="test 2.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 1)
    collector.add(CloseContext(), 0)
    collector.add(StartContext())
    collector.add(StartTestcase(description="test 22"))
    collector.add(StartTest(expected="test 1.1"))
    collector.add(
        CloseTest(generated="test 1.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 0)
    collector.add(CloseContext(), 1)
    collector.add(CloseTab(), 0)

    # The second tab should now be added.
    terminate(bundle, collector, status_if_unclosed=Status.RUNTIME_ERROR)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    assert updates.find_status_enum() == [
        "correct",
        "correct",
        "correct",
        "runtime error",
        "wrong",
    ]


def test_mid_context_is_completed(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, TEST_SUITE)
    collector = OutputManager(out=result)

    # Only the first tab has been executed.
    collector.add(StartJudgement())
    collector.add(StartTab(TEST_SUITE.tabs[0].name))
    collector.add(StartContext())
    collector.add(StartTestcase(description="test 1"))
    collector.add(StartTest(expected="test 1.1"))
    collector.add(
        CloseTest(generated="test 1.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 0)

    # The second tab should now be added.
    terminate(bundle, collector, status_if_unclosed=Status.RUNTIME_ERROR)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    assert updates.find_status_enum() == [
        "correct",
        "runtime error",
        "wrong",
        "wrong",
        "wrong",
    ]


def test_mid_tab_is_completed(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, TEST_SUITE)
    collector = OutputManager(out=result)

    # Only the first tab has been executed.
    collector.add(StartJudgement())
    collector.add(StartTab(TEST_SUITE.tabs[0].name))
    collector.add(StartContext())
    collector.add(StartTestcase(description="test 1"))
    collector.add(StartTest(expected="test 1.1"))
    collector.add(
        CloseTest(generated="test 1.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 0)
    collector.add(StartTestcase(description="test 2"))
    collector.add(StartTest(expected="test 2.1"))
    collector.add(
        CloseTest(generated="test 2.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 1)
    collector.add(CloseContext(), 0)

    # The second tab should now be added.
    terminate(bundle, collector, status_if_unclosed=Status.RUNTIME_ERROR)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    assert updates.find_status_enum() == [
        "correct",
        "correct",
        "runtime error",
        "wrong",
        "wrong",
    ]


def _add_passed_first_tab(collector: OutputManager):
    collector.add(StartJudgement())
    collector.add(StartTab(TEST_SUITE.tabs[0].name))
    collector.add(StartContext())
    collector.add(StartTestcase(description="test 1"))
    collector.add(StartTest(expected="test 1.1"))
    collector.add(
        CloseTest(generated="test 1.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 0)
    collector.add(StartTestcase(description="test 2"))
    collector.add(StartTest(expected="test 2.1"))
    collector.add(
        CloseTest(generated="test 2.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 1)
    collector.add(CloseContext(), 0)
    collector.add(StartContext())
    collector.add(StartTestcase(description="test 22"))
    collector.add(StartTest(expected="test 1.1"))
    collector.add(
        CloseTest(generated="test 1.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 0)
    collector.add(CloseContext(), 1)
    collector.add(CloseTab(), 0)


def _add_passed_second_tab(collector: OutputManager):
    collector.add(StartTab(TEST_SUITE.tabs[1].name))
    collector.add(StartContext())
    collector.add(StartTestcase(description="test 2.1"))
    collector.add(StartTest(expected="test 2.1.1"))
    collector.add(
        CloseTest(generated="test 2.1.2", status=StatusMessage(enum=Status.CORRECT))
    )
    collector.add(CloseTestcase(), 0)
    collector.add(CloseContext(), 0)
    collector.add(CloseTab(), 1)


def test_time_limit_closes_judgement_not_accepted(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, TEST_SUITE)
    collector = OutputManager(out=result)

    # Every context that ran passed; the second tab never ran.
    _add_passed_first_tab(collector)
    terminate(bundle, collector, status_if_unclosed=Status.TIME_LIMIT_EXCEEDED)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    close = updates.find_next("close-judgement")
    assert close["accepted"] is False


def test_time_limit_in_last_tab_closes_judgement_not_accepted(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, TEST_SUITE)
    collector = OutputManager(out=result)

    # Every context that ran passed, and the last tab is open when time runs out.
    _add_passed_first_tab(collector)
    _add_passed_second_tab(collector)
    terminate(bundle, collector, status_if_unclosed=Status.TIME_LIMIT_EXCEEDED)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    close = updates.find_next("close-judgement")
    assert close["accepted"] is False


def test_correct_termination_leaves_accepted_to_dodona(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, TEST_SUITE)
    collector = OutputManager(out=result)

    _add_passed_first_tab(collector)
    _add_passed_second_tab(collector)
    terminate(bundle, collector, status_if_unclosed=Status.CORRECT)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    assert "accepted" not in updates.find_next("close-judgement")


def test_unsupported_language_closes_judgement_not_accepted(
    tmp_path: Path, pytestconfig: pytest.Config, mocker
):
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, TEST_SUITE)
    mocker.patch("tested.judge.core.is_supported", return_value=["Unsupported."])

    judge(bundle)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    close = updates.find_next("close-judgement")
    assert close["status"]["enum"] == "internal error"
    assert close["accepted"] is False


FILE_OUTPUT_SUITE = Suite(
    tabs=[
        Tab(
            name="Tab 1",
            contexts=[
                Context(
                    testcases=[
                        Testcase(
                            input=MainInput(arguments=["hello 1"]),
                            output=Output(
                                file=FileOutputChannel(
                                    files=[
                                        TextData(path="out1.txt", content="expected1"),
                                        TextData(path="out2.txt", content="expected2"),
                                    ]
                                )
                            ),
                        ),
                    ]
                ),
            ],
        ),
    ]
)


def test_complete_evaluation_with_file_output(
    tmp_path: Path, pytestconfig: pytest.Config
):
    """Test that complete_evaluation emits per-file NOT_EXECUTED tests."""
    conf = configuration(pytestconfig, "", SupportedLanguage.JAVASCRIPT, tmp_path)
    result = StringIO()
    bundle = create_bundle(conf, result, FILE_OUTPUT_SUITE)
    collector = OutputManager(out=result)

    # Open judgement but don't execute anything.
    collector.add(StartJudgement())

    terminate(bundle, collector, status_if_unclosed=Status.RUNTIME_ERROR)

    updates = assert_valid_output(result.getvalue(), pytestconfig)

    # Each file should produce its own NOT_EXECUTED test.
    start_tests = updates.find_all("start-test")
    assert len(start_tests) == 2
    assert start_tests[0]["channel"] == "out1.txt"
    assert start_tests[1]["channel"] == "out2.txt"

    assert updates.find_status_enum() == [
        "runtime error",
        "wrong",
        "wrong",
    ]


def test_file_output_channel_rejects_none_path():
    """Test that FileOutputChannel rejects files with path=None."""
    with pytest.raises(ValueError, match="File path must be set"):
        FileOutputChannel(
            files=[
                TextData(path=None, content="expected1"),
                TextData(path="out2.txt", content="expected2"),
            ]
        )


def test_compiler_annotations_are_added_once():
    result = StringIO()
    collector = OutputManager(out=result)
    annotation = AnnotateCode(row=1, column=2, text="first")
    other = AnnotateCode(row=1, column=2, text="second")

    collector.add_compiler_annotations([annotation, other])
    collector.add_compiler_annotations([AnnotateCode(row=1, column=2, text="first")])

    assert result.getvalue().count('"command": "annotate-code"') == 2
