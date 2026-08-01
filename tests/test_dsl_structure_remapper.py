import copy

import pytest

from tested.dsl.structure_remapper import remap_structure
from tested.dsl.translate_parser import parse_dsl


def _remapped(dsl_object):
    remap_structure(dsl_object)
    return dsl_object


def test_remap_root_mapping_with_scripts():
    dsl_object = {"units": [{"unit": "T", "scripts": [{"expression": "test()"}]}]}
    assert _remapped(dsl_object) == {
        "tabs": [{"tab": "T", "testcases": [{"expression": "test()"}]}]
    }


def test_remap_root_mapping_with_cases():
    dsl_object = {
        "units": [{"unit": "T", "cases": [{"script": [{"expression": "test()"}]}]}]
    }
    assert _remapped(dsl_object) == {
        "tabs": [{"tab": "T", "contexts": [{"testcases": [{"expression": "test()"}]}]}]
    }


def test_remap_top_level_list_with_scripts():
    dsl_object = [{"unit": "T", "scripts": [{"expression": "test()"}]}]
    assert _remapped(dsl_object) == [
        {"tab": "T", "testcases": [{"expression": "test()"}]}
    ]


def test_remap_top_level_list_with_cases():
    dsl_object = [{"unit": "T", "cases": [{"script": [{"expression": "test()"}]}]}]
    assert _remapped(dsl_object) == [
        {"tab": "T", "contexts": [{"testcases": [{"expression": "test()"}]}]}
    ]


def test_remap_keeps_other_fields():
    dsl_object = {
        "namespace": "solution",
        "units": [{"unit": "T", "hidden": True, "scripts": [{"expression": "test()"}]}],
    }
    assert _remapped(dsl_object) == {
        "namespace": "solution",
        "tabs": [{"tab": "T", "hidden": True, "testcases": [{"expression": "test()"}]}],
    }


def test_remap_does_not_descend_into_testcases():
    # A testcase may legitimately contain keys that the remapper renames at
    # other levels; it must not touch them.
    dsl_object = [
        {"unit": "T", "scripts": [{"expression": "test()", "definitions": {"unit": 5}}]}
    ]
    testcase = _remapped(dsl_object)[0]["testcases"][0]
    assert testcase["definitions"] == {"unit": 5}


@pytest.mark.parametrize(
    "dsl_object",
    [
        pytest.param({"tabs": [{"tab": "T", "testcases": []}]}, id="root-mapping"),
        pytest.param([{"tab": "T", "testcases": []}], id="top-level-list"),
        pytest.param(
            [{"tab": "T", "contexts": [{"testcases": []}]}], id="with-contexts"
        ),
    ],
)
def test_remap_leaves_canonical_suites_alone(dsl_object):
    original = copy.deepcopy(dsl_object)
    assert _remapped(dsl_object) == original


@pytest.mark.parametrize(
    "dsl_object",
    [
        pytest.param([], id="empty-list"),
        pytest.param(None, id="null-document"),
        pytest.param("nonsense", id="scalar-document"),
        pytest.param(42, id="number-document"),
        pytest.param(["a", "b"], id="list-of-non-dicts"),
        pytest.param({}, id="empty-mapping"),
        pytest.param({"units": "oops"}, id="units-not-a-list"),
        pytest.param([{"unit": "T"}], id="unit-without-children"),
        pytest.param([{"unit": "T", "cases": "oops"}], id="cases-not-a-list"),
        pytest.param([{"unit": "T", "cases": ["oops"]}], id="context-not-a-dict"),
        pytest.param([{"unit": "T", "scripts": "oops"}], id="scripts-not-a-list"),
        pytest.param(["oops", {"unit": "T", "scripts": []}], id="mixed-list-items"),
    ],
)
def test_remap_never_raises_on_malformed_input(dsl_object):
    # Malformed suites must survive the remapper untouched so that the schema,
    # not a stack trace, is what the user ends up seeing.
    remap_structure(dsl_object)


_EQUIVALENT_SUITES = [
    pytest.param(
        """
- unit: 'T'
  scripts:
    - expression: 'test()'
      return: 5
""",
        """
- tab: 'T'
  testcases:
    - expression: 'test()'
      return: 5
""",
        id="top-level-list-scripts",
    ),
    pytest.param(
        """
- unit: 'T'
  cases:
    - script:
        - expression: 'test()'
          return: 5
""",
        """
- tab: 'T'
  contexts:
    - testcases:
        - expression: 'test()'
          return: 5
""",
        id="top-level-list-cases",
    ),
    pytest.param(
        """
units:
  - unit: 'T'
    scripts:
      - expression: 'test()'
        return: 5
""",
        """
tabs:
  - tab: 'T'
    testcases:
      - expression: 'test()'
        return: 5
""",
        id="root-mapping-scripts",
    ),
    pytest.param(
        """
units:
  - unit: 'T'
    cases:
      - script:
          - expression: 'test()'
            return: 5
""",
        """
tabs:
  - tab: 'T'
    contexts:
      - testcases:
          - expression: 'test()'
            return: 5
""",
        id="root-mapping-cases",
    ),
]


@pytest.mark.parametrize("alternative,canonical", _EQUIVALENT_SUITES)
def test_alternative_structure_parses_identically(alternative, canonical):
    assert parse_dsl(alternative) == parse_dsl(canonical)
