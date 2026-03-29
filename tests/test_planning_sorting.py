import pytest

from tested.judge.planning import PlannedContext, PlannedExecutionUnit
from tested.testsuite import (
    ContentPath,
    Context,
    EmptyChannel,
    MainInput,
    Testcase,
    TextData,
)


def test_get_dynamically_generated_files_stdin_not_included():
    # stdin.path is display-only ("< hello.txt" in the description).
    # Stdin content is delivered via the process pipe, not written to disk.
    file1 = TextData(path="stdin.txt", content=ContentPath(path="url1"))
    testcase1 = Testcase(input=MainInput(stdin=file1), input_files=[])
    context1 = Context(testcases=[testcase1])
    planned_context1 = PlannedContext(context=context1, tab_index=0, context_index=0)

    file2 = TextData(path="stdin.txt", content=ContentPath(path="url2"))
    testcase2 = Testcase(input=MainInput(stdin=file2), input_files=[])
    context2 = Context(testcases=[testcase2])
    planned_context2 = PlannedContext(context=context2, tab_index=0, context_index=1)

    unit = PlannedExecutionUnit(
        contexts=[planned_context1, planned_context2], name="unit0", index=0
    )

    assert unit.get_dynamically_generated_files() == []


def test_get_dynamically_generated_files_empty():
    """An execution unit with no contexts should return an empty list."""
    unit = PlannedExecutionUnit(contexts=[], name="empty_unit", index=0)
    assert unit.get_dynamically_generated_files() == []


def test_get_dynamically_generated_files_no_files():
    """Contexts with testcases but no input files or stdin should return empty."""
    testcase = Testcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])
    context = Context(testcases=[testcase])
    planned = PlannedContext(context=context, tab_index=0, context_index=0)

    unit = PlannedExecutionUnit(contexts=[planned], name="no_files_unit", index=0)
    assert unit.get_dynamically_generated_files() == []


def test_get_dynamically_generated_files_not_dynamic():
    """
    Files where the `path` matches the `ContentPath.path` are considered static
    by `TextData.is_dynamically_generated()` and should be ignored.
    """
    # static.txt maps exactly to the content path static.txt
    static_file = TextData(path="static.txt", content=ContentPath(path="static.txt"))

    testcase = Testcase(input=MainInput(stdin=static_file), input_files=[static_file])
    context = Context(testcases=[testcase])
    planned = PlannedContext(context=context, tab_index=0, context_index=0)

    unit = PlannedExecutionUnit(contexts=[planned], name="static_unit", index=0)
    assert unit.get_dynamically_generated_files() == []


def test_get_dynamically_generated_files_deduplication():
    file1 = TextData(path="data.txt", content="shared content")

    # Same file used in stdin and input_files
    testcase1 = Testcase(input=MainInput(stdin=file1), input_files=[file1])
    # Same file used in a completely different context
    testcase2 = Testcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1])

    planned1 = PlannedContext(
        context=Context(testcases=[testcase1]), tab_index=0, context_index=0
    )
    planned2 = PlannedContext(
        context=Context(testcases=[testcase2]), tab_index=0, context_index=1
    )

    unit = PlannedExecutionUnit(
        contexts=[planned1, planned2], name="dedup_unit", index=0
    )

    generated_files = unit.get_dynamically_generated_files()
    assert len(generated_files) == 1
    assert generated_files[0].path == "data.txt"
    assert generated_files[0].content == "shared content"


def test_get_dynamically_generated_files_complex_sorting():
    file_a1 = TextData(path="a.txt", content=ContentPath(path="z_url"))
    file_a2 = TextData(path="b.txt", content="a_string")
    file_a3 = TextData(path="c.txt", content=ContentPath(path="m_url"))

    testcase = Testcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[file_a1, file_a3, file_a2],
    )
    planned = PlannedContext(
        context=Context(testcases=[testcase]), tab_index=0, context_index=0
    )
    unit = PlannedExecutionUnit(contexts=[planned], name="sort_unit", index=0)

    generated = unit.get_dynamically_generated_files()

    assert len(generated) == 3

    assert generated[0].path == "a.txt"
    assert isinstance(generated[0].content, ContentPath)
    assert generated[0].content.path == "z_url"

    assert generated[1].path == "b.txt"
    assert generated[1].content == "a_string"

    assert generated[2].path == "c.txt"
    assert isinstance(generated[2].content, ContentPath)
    assert generated[2].content.path == "m_url"


def test_get_dynamically_generated_files_none_path():
    """
    If a file has no path assigned (e.g., stdout expectations),
    it should not be considered a dynamically generated input file.
    """
    # Path is None
    file_no_path = TextData(path=None, content="content")

    testcase = Testcase(input=MainInput(stdin=file_no_path), input_files=[file_no_path])
    planned = PlannedContext(
        context=Context(testcases=[testcase]), tab_index=0, context_index=0
    )
    unit = PlannedExecutionUnit(contexts=[planned], name="no_path_unit", index=0)

    assert unit.get_dynamically_generated_files() == []


@pytest.mark.parametrize(
    "text_data, expected",
    [
        (TextData(content="inline", path=None), False),
        (TextData(content="inline text", path="x.txt"), True),
        (TextData(content=ContentPath(path="x.txt"), path="x.txt"), False),
        (TextData(content=ContentPath(path="other.txt"), path="x.txt"), True),
    ],
)
def test_is_dynamically_generated(text_data: TextData, expected: bool):
    assert text_data.is_dynamically_generated() == expected


def test_get_dynamically_generated_files_strict_workdir_includes_static():
    """In strict mode, static ContentPath files (path == content.path) are included."""
    static_file = TextData(path="f.txt", content=ContentPath(path="f.txt"))
    testcase = Testcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[static_file],
        use_strict_workdir=True,
    )
    context = Context(testcases=[testcase])
    planned = PlannedContext(context=context, tab_index=0, context_index=0)
    unit = PlannedExecutionUnit(contexts=[planned], name="strict_unit", index=0)

    generated = unit.get_dynamically_generated_files()

    assert len(generated) == 1
    assert generated[0].path == "f.txt"
    assert isinstance(generated[0].content, ContentPath)
    assert generated[0].content.path == "f.txt"


def test_get_dynamically_generated_files_not_dynamic_lax_vs_strict():
    """Same static ContentPath file: excluded in lax mode, included in strict mode."""
    static_file = TextData(path="f.txt", content=ContentPath(path="f.txt"))

    # Lax: use_strict_workdir defaults to False
    lax_testcase = Testcase(
        input=MainInput(stdin=EmptyChannel.NONE), input_files=[static_file]
    )
    lax_context = Context(testcases=[lax_testcase])
    lax_planned = PlannedContext(context=lax_context, tab_index=0, context_index=0)
    lax_unit = PlannedExecutionUnit(contexts=[lax_planned], name="lax_unit", index=0)

    assert lax_unit.get_dynamically_generated_files() == []

    # Strict: use_strict_workdir=True
    strict_testcase = Testcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[static_file],
        use_strict_workdir=True,
    )
    strict_context = Context(testcases=[strict_testcase])
    strict_planned = PlannedContext(context=strict_context, tab_index=0, context_index=0)
    strict_unit = PlannedExecutionUnit(
        contexts=[strict_planned], name="strict_unit", index=0
    )

    assert len(strict_unit.get_dynamically_generated_files()) == 1


def test_has_strict_workdir():
    """has_strict_workdir() returns True when any context in the unit is strict."""
    strict_testcase = Testcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[],
        use_strict_workdir=True,
    )
    lax_testcase = Testcase(input=MainInput(stdin=EmptyChannel.NONE), input_files=[])

    strict_context = Context(testcases=[strict_testcase])
    lax_context = Context(testcases=[lax_testcase])

    strict_planned = PlannedContext(context=strict_context, tab_index=0, context_index=0)
    lax_planned = PlannedContext(context=lax_context, tab_index=0, context_index=1)

    mixed_unit = PlannedExecutionUnit(
        contexts=[lax_planned, strict_planned], name="mixed_unit", index=0
    )
    assert mixed_unit.has_strict_workdir() is True

    all_lax_unit = PlannedExecutionUnit(
        contexts=[lax_planned], name="lax_unit", index=1
    )
    assert all_lax_unit.has_strict_workdir() is False
