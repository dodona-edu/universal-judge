import sys
from pathlib import Path

import pytest

from tested.configs import create_bundle
from tested.dodona import ExtendedMessage
from tested.judge.evaluation import link_files_message
from tested.languages.generation import get_readable_input
from tested.testsuite import (
    ContentPath,
    Context,
    MainInput,
    Suite,
    Tab,
    Testcase,
    TextData,
)
from tests.manual_utils import configuration


def test_link_files_message_single_file():
    link_files = [
        TextData(path="data.txt", content=ContentPath(path="path/to/data.txt"))
    ]
    message = link_files_message(link_files)

    assert isinstance(message.message, ExtendedMessage)
    assert message.message.format == "html"
    assert 'href="path/to/data.txt"' in message.message.description
    assert "data.txt</span></a>" in message.message.description
    assert "contains-file" in message.message.description


def test_link_files_message_multiple_files():
    link_files = [
        TextData(path="file1.txt", content=ContentPath(path="url1")),
        TextData(path="file2.txt", content=ContentPath(path="url2")),
    ]

    message = link_files_message(link_files)

    assert isinstance(message.message, ExtendedMessage)
    assert 'href="url1"' in message.message.description
    assert "file1.txt</span></a>" in message.message.description
    assert 'href="url2"' in message.message.description
    assert "file2.txt</span></a>" in message.message.description
    # It should be a comma-separated list
    assert ", " in message.message.description


def test_link_files_message_inline_content_ignored():
    link_files = [
        TextData(path="inline.txt", content="some content"),
        TextData(path="linked.txt", content=ContentPath(path="linked-url")),
    ]
    message = link_files_message(link_files)

    assert isinstance(message.message, ExtendedMessage)
    assert 'href="linked-url"' in message.message.description
    assert "linked.txt</span></a>" in message.message.description
    assert "inline.txt" not in message.message.description


def test_link_files_message_no_path_ignored():
    link_files = [TextData(content=ContentPath(path="some-url"))]
    message = link_files_message(link_files)

    # If path is None, it should be ignored by link_files_message
    assert isinstance(message.message, ExtendedMessage)
    assert "href=" not in message.message.description


def test_readable_input_file_linking(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan.yaml",
        "correct",
    )

    # Testcase with a file in arguments
    the_input = Testcase(
        input=MainInput(arguments=["data.txt"], stdin=TextData(content="ignored\n")),
        input_files=[
            TextData(path="data.txt", content=ContentPath(path="path/to/data.txt"))
        ],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)
    readable, seen = get_readable_input(bundle, the_input)

    assert 'href="path/to/data.txt"' in readable.description
    assert "data.txt</a>" in readable.description
    assert the_input.input_files[0] in seen


def test_readable_input_multiple_files(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan.yaml",
        "correct",
    )

    the_input = Testcase(
        input=MainInput(
            arguments=["file1.txt", "file2.txt"],
        ),
        input_files=[
            TextData(path="file1.txt", content=ContentPath(path="url1")),
            TextData(path="file2.txt", content=ContentPath(path="url2")),
        ],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)
    readable, seen = get_readable_input(bundle, the_input)

    assert 'href="url1"' in readable.description
    assert 'href="url2"' in readable.description
    assert len(seen) == 2


def test_readable_input_stdin_file(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan.yaml",
        "correct",
    )

    the_input = Testcase(
        input=MainInput(
            arguments=[],
            stdin=TextData(path="input.txt", content=ContentPath(path="input-url")),
        ),
        input_files=[TextData(path="input.txt", content=ContentPath(path="input-url"))],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)
    readable, seen = get_readable_input(bundle, the_input)

    # When stdin has a path, the description is "$ submission < input.txt" (or similar)
    # We want to check if "input.txt" is linked.
    assert 'href="input-url"' in readable.description
    assert "input.txt</a>" in readable.description
    assert the_input.input_files[0] in seen


def test_readable_input_no_match(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan.yaml",
        "correct",
    )

    the_input = Testcase(
        input=MainInput(
            arguments=["something-else.txt"],
        ),
        input_files=[
            TextData(path="data.txt", content=ContentPath(path="path/to/data.txt"))
        ],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)
    readable, seen = get_readable_input(bundle, the_input)

    # No link should be generated because filenames don't match -> dynamic file.
    assert "href=" not in readable.description
    assert len(seen) == 0


def test_readable_input_inline_content_no_link(
    tmp_path: Path, pytestconfig: pytest.Config
):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan.yaml",
        "correct",
    )

    the_input = Testcase(
        input=MainInput(
            arguments=["data.txt"],
        ),
        input_files=[TextData(path="data.txt", content="inline content")],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)
    readable, seen = get_readable_input(bundle, the_input)

    # Inline content should NOT be linked currently
    assert "href=" not in readable.description
    assert "data.txt" in readable.description
    assert len(seen) == 0


def test_readable_input_legacy_files(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig,
        "echo",
        "python",
        tmp_path,
        "plan.yaml",
        "correct",
    )

    the_input = Testcase(
        input=MainInput(
            arguments=["legacy.txt"],
        ),
        input_files=[
            TextData(path="legacy.txt", content=ContentPath(path="legacy-url"))
        ],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)
    readable, seen = get_readable_input(bundle, the_input)

    assert 'href="legacy-url"' in readable.description
    assert "legacy.txt</a>" in readable.description
    assert len(seen) == 1
