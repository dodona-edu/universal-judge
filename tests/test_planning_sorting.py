from tested.judge.planning import PlannedContext, PlannedExecutionUnit
from tested.testsuite import (
    ContentPath,
    Context,
    EmptyChannel,
    MainInput,
    Testcase,
    TextData,
)


def test_get_dynamically_generated_files_no_type_error():
    file1 = TextData(path="same.txt", content=ContentPath(path="url1"))
    file2 = TextData(path="same.txt", content=ContentPath(path="url2"))
    file3 = TextData(path="same.txt", content="plain text content")

    testcase1 = Testcase(
        input=MainInput(stdin=EmptyChannel.NONE), input_files=[file1, file2, file3]
    )

    context = Context(testcases=[testcase1])
    planned_context = PlannedContext(context=context, tab_index=0, context_index=0)

    unit = PlannedExecutionUnit(contexts=[planned_context], name="unit0", index=0)

    generated_files = unit.get_dynamically_generated_files()

    assert len(generated_files) == 3
    # Check that sorting actually happened based on path and then content string representation
    # "plain text content" comes before "url1" and "url2" alphabetically
    assert generated_files[0].content == "plain text content"
    assert isinstance(generated_files[1].content, ContentPath)
    assert generated_files[1].content.path == "url1"
    assert isinstance(generated_files[2].content, ContentPath)
    assert generated_files[2].content.path == "url2"


def test_get_dynamically_generated_files_stdin_sorting():
    # Test sorting when files are generated from stdin
    # We must put them in different contexts because only the first testcase
    # in a context can have MainInput.
    file1 = TextData(path="stdin.txt", content=ContentPath(path="url1"))
    testcase1 = Testcase(input=MainInput(stdin=file1), input_files=[])
    context1 = Context(testcases=[testcase1])
    planned_context1 = PlannedContext(context=context1, tab_index=0, context_index=0)

    # Another context with same stdin path but different url
    file2 = TextData(path="stdin.txt", content=ContentPath(path="url2"))
    testcase2 = Testcase(input=MainInput(stdin=file2), input_files=[])
    context2 = Context(testcases=[testcase2])
    planned_context2 = PlannedContext(context=context2, tab_index=0, context_index=1)

    unit = PlannedExecutionUnit(
        contexts=[planned_context1, planned_context2], name="unit0", index=0
    )

    generated_files = unit.get_dynamically_generated_files()
    assert len(generated_files) == 2
    assert isinstance(generated_files[0].content, ContentPath)
    assert generated_files[0].content.path == "url1"
    assert isinstance(generated_files[1].content, ContentPath)
    assert generated_files[1].content.path == "url2"


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
    # Group A: Same path "a.txt", different contents to test secondary sort
    file_a1 = TextData(path="a.txt", content=ContentPath(path="z_url"))
    file_a2 = TextData(path="a.txt", content="a_string")
    file_a3 = TextData(path="a.txt", content=ContentPath(path="m_url"))

    # Group B: Different path "b.txt" to test primary sort
    file_b1 = TextData(path="b.txt", content="b_string")

    testcase = Testcase(
        input=MainInput(stdin=EmptyChannel.NONE),
        input_files=[file_b1, file_a1, file_a3, file_a2],
    )
    planned = PlannedContext(
        context=Context(testcases=[testcase]), tab_index=0, context_index=0
    )
    unit = PlannedExecutionUnit(contexts=[planned], name="sort_unit", index=0)

    generated = unit.get_dynamically_generated_files()

    assert len(generated) == 4

    # Expected order:
    # 1. a.txt -> "a_string"
    # 2. a.txt -> m_url
    # 3. a.txt -> z_url
    # 4. b.txt -> "b_string"

    assert generated[0].path == "a.txt"
    assert generated[0].content == "a_string"

    assert generated[1].path == "a.txt"
    assert isinstance(generated[1].content, ContentPath)
    assert generated[1].content.path == "m_url"

    assert generated[2].path == "a.txt"
    assert isinstance(generated[2].content, ContentPath)
    assert generated[2].content.path == "z_url"

    assert generated[3].path == "b.txt"
    assert generated[3].content == "b_string"


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
