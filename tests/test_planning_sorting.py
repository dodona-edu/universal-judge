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
