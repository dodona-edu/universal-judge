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

    assert message
    assert isinstance(message.message, ExtendedMessage)
    assert message.message.format == "text"
    assert "data.txt" in message.message.description


def test_link_files_message_multiple_files():
    link_files = [
        TextData(path="file1.txt", content=ContentPath(path="url1")),
        TextData(path="file2.txt", content=ContentPath(path="url2")),
    ]

    message = link_files_message(link_files)

    assert message
    assert isinstance(message.message, ExtendedMessage)
    assert message.message.format == "text"
    assert "file1.txt" in message.message.description
    assert "file2.txt" in message.message.description
    # It should be a comma-separated list
    assert ", " in message.message.description


def test_link_files_message_includes_inline_files():
    link_files = [
        TextData(path="inline.txt", content="some content"),
        TextData(path="linked.txt", content=ContentPath(path="linked-url")),
    ]
    message = link_files_message(link_files)

    assert message
    assert isinstance(message.message, ExtendedMessage)
    assert "inline.txt" in message.message.description
    assert "linked.txt" in message.message.description


def test_link_files_message_no_path_ignored():
    link_files = [TextData(content=ContentPath(path="some-url"))]
    message = link_files_message(link_files)

    # If path is None, it should be ignored by link_files_message
    assert message is None


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

    assert "data.txt" in readable.description
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

    assert "file1.txt" in readable.description
    assert "file2.txt" in readable.description
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

    assert "input.txt" in readable.description
    assert the_input.input_files[0] in seen


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

    assert "data.txt" in readable.description
    assert len(seen) == 1


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

    assert "legacy.txt" in readable.description
    assert len(seen) == 1


def test_link_files_message_emits_raw_filenames():
    # URL-encoding of the link target now happens on the server. TESTed lists the
    # filenames verbatim, including spaces and non-ASCII characters.
    link_files = [
        TextData(path="my file.txt", content=ContentPath(path="path/to/my file.txt")),
        TextData(path="résumé.txt", content=ContentPath(path="files/résumé.txt")),
    ]
    message = link_files_message(link_files)
    assert message is not None
    assert isinstance(message.message, ExtendedMessage)
    desc = message.message.description
    assert "my file.txt" in desc
    assert "résumé.txt" in desc
    assert "href=" not in desc
    assert "%" not in desc


def test_get_display_path_with_override():
    td = TextData(
        path="getallen1.txt",
        content=ContentPath(
            path="getallen1.txt", display_override="media/getallen1.txt"
        ),
    )
    assert td.get_display_path() == "media/getallen1.txt"


def test_get_display_path_content_path_no_override():
    td = TextData(path="data.txt", content=ContentPath(path="path/to/data.txt"))
    assert td.get_display_path() == "media/path/to/data.txt"


def test_get_display_path_inline_content():
    td = TextData(path="inline.txt", content="some inline content")
    assert td.get_display_path() is None


def test_readable_input_with_display_override(
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
        input=MainInput(arguments=["getallen1.txt"]),
        input_files=[
            TextData(
                path="getallen1.txt",
                content=ContentPath(
                    path="getallen1.txt",
                    display_override="media/getallen1.txt",
                ),
            )
        ],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)
    readable, seen = get_readable_input(bundle, the_input)

    assert "getallen1.txt" in readable.description
    assert the_input.input_files[0] in seen


def test_readable_input_no_path_in_input_files(
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

    # Testcase with an input file that has NO path.
    the_input = Testcase(
        input=MainInput(
            arguments=["some-arg"],
        ),
        input_files=[TextData(path=None, content="some content")],
    )

    suite = Suite(tabs=[Tab(contexts=[Context(testcases=[the_input])], name="test")])
    bundle = create_bundle(conf, sys.stdout, suite)

    readable, seen = get_readable_input(bundle, the_input)

    assert "some-arg" in readable.description
    assert len(seen) == 0
