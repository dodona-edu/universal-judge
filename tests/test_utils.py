import json
from pathlib import Path

import pytest
import yaml

from tested.datatypes import (
    AdvancedNothingTypes,
    AdvancedSequenceTypes,
    BasicNumericTypes,
)
from tested.serialisation import NothingType, NumberType, SequenceType
from tested.utils import sorted_no_duplicates, sorting_value_extract
from tests.manual_utils import assert_valid_output, configuration, execute_config


def test_javascript_ast_parse():
    expected = frozenset(
        [
            "readFileSync",
            "a",
            "b",
            "x",
            "y",
            "demoFunction",
            "SimpleClass",
            "StaticClass",
            "tryCatch",
            "z",
            "asyncFunction",
        ]
    )
    from tested.judge.utils import run_command

    test_dir = Path(__file__).parent
    parse_file = test_dir.parent / "tested" / "languages" / "javascript" / "parseAst.js"
    demo_file = test_dir / "testJavascriptAstParserFile.js"
    output = run_command(
        demo_file.parent,
        timeout=None,
        command=["node", str(parse_file), str(demo_file.absolute())],
    )
    assert output
    namings = frozenset(output.stdout.strip().split(", "))
    assert namings == expected


def test_typescript_ast_parse():
    expected = frozenset(
        [
            "c",
            "d",
            "a",
            "b",
            "x",
            "y",
            "demoFunction",
            "SimpleClass",
            "StaticClass",
            "tryCatch",
            "z",
            "asyncFunction",
        ]
    )
    from tested.judge.utils import run_command

    test_dir = Path(__file__).parent
    parse_file = test_dir.parent / "tested" / "languages" / "typescript" / "parseAst.js"
    demo_file = test_dir / "testTypeScriptAstParserFile.ts"
    output = run_command(
        demo_file.parent,
        timeout=None,
        command=["tsx", str(parse_file), str(demo_file.absolute())],
    )
    assert output
    namings = frozenset(output.stdout.strip().split(", "))
    assert namings == expected


def test_run_doctests_tested_utils():
    import doctest

    import tested.utils

    f, _ = doctest.testmod(tested.utils)
    assert f == 0


def test_run_doctests_tested_conventionalize():
    import doctest

    import tested.languages.conventionalize

    f, _ = doctest.testmod(tested.languages.conventionalize)
    assert f == 0


def test_sort_no_duplicates():
    data = [
        "a",
        5,
        8,
        3,
        7,
        6,
        28,
        "b",
        5,
        (True, False),
        ("a", ("b", "c")),
        "data",
        0,
        1,
        2,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30,
        31,
        32,
        "a",
        5,
        8,
        3,
        7,
        6,
        28,
        "b",
        5,
        (True, False),
        ("a", ("b", "c")),
        "data",
        0,
        1,
        2,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30,
        31,
        32,
    ]
    expected = [
        0,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9,
        10,
        11,
        12,
        13,
        14,
        15,
        16,
        17,
        18,
        19,
        20,
        21,
        22,
        23,
        24,
        25,
        26,
        27,
        28,
        29,
        30,
        31,
        32,
        "a",
        "b",
        "data",
        (True, False),
        ("a", ("b", "c")),
    ]
    result = sorted_no_duplicates(data)
    assert expected == result


def test_sort_key():
    data = [("alpha", 5), (1, 0), (8.4, "beta")]
    expected = [(8.4, "beta"), (1, 0), ("alpha", 5)]
    result = sorted_no_duplicates(data, key=lambda x: x[0])
    assert expected == result


def test_sort_recursive():
    data = [(("alpha", 5), 5), ((1, 0), 0), ((1, "beta"), "beta")]
    expected = [((1, 0), 0), (("alpha", 5), 5), ((1, "beta"), "beta")]
    result = sorted_no_duplicates(
        data, key=lambda x: x[0], recursive_key=lambda x: x[1]
    )
    assert expected == result


def test_sort_empty():
    assert [] == sorted_no_duplicates([])


def test_can_sort_nested_sets():
    value = SequenceType(
        type=AdvancedSequenceTypes.LIST,
        data=[
            NumberType(type=BasicNumericTypes.INTEGER, data=5, diagnostic=None),
            NothingType(
                type=AdvancedNothingTypes.UNDEFINED, data=None, diagnostic=None
            ),
        ],
        diagnostic=None,
    )
    result = sorted_no_duplicates([value, value], key=sorting_value_extract)
    assert [value] == result


def test_valid_yaml_and_json():
    """
    Test to validate if all YAML and JSON can be parsed correctly.
    """

    def recursive_iter_dir(directory: Path) -> list[Path]:
        yaml_and_json_files = []
        for file in directory.iterdir():
            if (
                file.is_file()
                and not file.name.startswith("tsconfig")
                and (
                    file.name.endswith(".yml")
                    or file.name.endswith(".yaml")
                    or file.name.endswith(".json")
                )
            ):
                yaml_and_json_files.append(file)
            elif file.is_dir():
                yaml_and_json_files.extend(recursive_iter_dir(file))
        return yaml_and_json_files

    data_dir = (Path(__file__).parent / ".." / "tested").resolve()
    files = recursive_iter_dir(data_dir)
    for f in files:
        # Parse to validate
        with open(f, "r") as fd:
            if f.name.endswith(".json"):
                json.load(fd)
            else:
                yaml.safe_load(fd)
    # Test will always succeed if no exception is thrown
    assert True


def test_invalid_utf8_output_is_caught(tmp_path: Path, pytestconfig: pytest.Config):
    conf = configuration(
        pytestconfig, "sum", "bash", tmp_path, "short.tson", "non-utf8-output"
    )
    result = execute_config(conf)
    updates = assert_valid_output(result, pytestconfig)
    assert updates.find_status_enum() == ["wrong"] * 5
