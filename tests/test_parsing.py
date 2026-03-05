from decimal import Decimal

import pytest
from attrs import define

from tested.parsing import fallback_field, get_converter, ignore_field


@define
class DummyClass:
    field1: str
    field2: int = 0


def test_ignore_field():
    @ignore_field("to_ignore")
    @define
    class IgnoreDummy:
        field1: str
        field2: int = 0

    converter = get_converter()
    data = {"field1": "value", "field2": 10, "to_ignore": "some_value"}
    result = converter.structure(data, IgnoreDummy)
    assert result.field1 == "value"
    assert result.field2 == 10


def test_ignore_multiple_fields():
    @ignore_field("ignore1", "ignore2")
    @define
    class IgnoreMultipleDummy:
        field1: str

    converter = get_converter()
    data = {"field1": "value", "ignore1": 1, "ignore2": 2}
    result = converter.structure(data, IgnoreMultipleDummy)
    assert result.field1 == "value"


def test_fallback_field():
    @fallback_field({"old_field": "new_field"})
    @define
    class FallbackDummy:
        new_field: str

    converter = get_converter()
    data = {"old_field": "value"}
    result = converter.structure(data, FallbackDummy)
    assert result.new_field == "value"

    data = {"old_field": "old", "new_field": "new"}
    result = converter.structure(data, FallbackDummy)
    assert result.new_field == "new"


def test_fallback_field_custom():
    def mapper(old_val, _d):
        return f"mapped_{old_val}"

    @fallback_field({"old": ("new", mapper)})
    @define
    class CustomFallbackDummy:
        new: str

    converter = get_converter()
    data = {"old": "value"}
    result = converter.structure(data, CustomFallbackDummy)
    assert result.new == "mapped_value"

    data = {"old": "old", "new": "new"}
    with pytest.raises(ValueError) as exc_info:
        converter.structure(data, CustomFallbackDummy)
    assert "You cannot use new and old simultaneously" in str(exc_info.value)


def test_fallback_field_custom_with_context():
    def mapper(old_val, d):
        return f"{old_val}_{d.get('other', 'none')}"

    @fallback_field({"old": ("new", mapper)})
    @define
    class CustomFallbackContextDummy:
        new: str
        other: str = "default"

    converter = get_converter()
    data = {"old": "value", "other": "context"}
    result = converter.structure(data, CustomFallbackContextDummy)
    assert result.new == "value_context"


def test_decorator_stacking():
    @fallback_field(
        {
            "old_custom": ("field1", lambda x, _: f"custom_{x}"),
            "old_fallback": "field2",
        }
    )
    @ignore_field("to_ignore")
    @define
    class StackedDummy:
        field1: str
        field2: int
        field3: str = "default"

    converter = get_converter()
    data = {
        "old_custom": "val1",
        "old_fallback": 42,
        "to_ignore": "hidden",
        "field3": "provided",
    }
    result = converter.structure(data, StackedDummy)
    assert result.field1 == "custom_val1"
    assert result.field2 == 42
    assert result.field3 == "provided"


def test_non_dict_input():
    @ignore_field("field")
    @define
    class NonDictDummy:
        field: str

    converter = get_converter()
    # If input is not a dict, it should pass through to previous hook (which might fail or handle it)
    # Actually, cattrs' structure for a class usually expects a dict if it's using make_dict_structure_fn
    with pytest.raises(Exception):
        converter.structure("not a dict", NonDictDummy)


def test_chain_structure_hook_registry_isolation():
    # Verify that decorators on one class don't affect another
    @ignore_field("extra")
    @define
    class ClassA:
        name: str

    @define
    class ClassB:
        name: str
        extra: str

    converter = get_converter()

    # ClassA should ignore "extra"
    data_a = {"name": "A", "extra": "ignored"}
    result_a = converter.structure(data_a, ClassA)
    assert result_a.name == "A"

    # ClassB should NOT ignore "extra" (it's a required field here)
    data_b = {"name": "B", "extra": "present"}
    result_b = converter.structure(data_b, ClassB)
    assert result_b.name == "B"
    assert result_b.extra == "present"


def test_decimal_hooks():
    from tested.parsing import structure_decimal, unstructure_decimal

    d = Decimal("1.23")
    assert unstructure_decimal(d) == "1.23"
    assert structure_decimal("1.23", Decimal) == d
    assert structure_decimal(1.23, Decimal) == d


def test_structure_every_union():
    from typing import Union

    from tested.parsing import structure_every_union
    from tested.serialisation import Identifier

    # Test Union[int, str]
    the_type = Union[int, str]
    assert structure_every_union(1, the_type) == 1
    assert structure_every_union("a", the_type) == "a"

    # Test Union[None, int]
    the_type = Union[None, int]
    assert structure_every_union(None, the_type) is None
    assert structure_every_union(5, the_type) == 5

    # Test Union[bool, int]
    the_type = Union[bool, int]
    assert structure_every_union(True, the_type) is True
    assert structure_every_union(0, the_type) == 0

    # Test Identifier in Union
    the_type = Union[Identifier, int]
    result = structure_every_union("my_id", the_type)
    assert isinstance(result, Identifier)
    assert result == "my_id"

    # Test failure
    with pytest.raises(TypeError):
        # We need something that doesn't match any branch in structure_every_union
        # The branches are:
        # 1. None
        # 2. bool
        # 3. int
        # 4. float
        # 5. Identifier (str)
        # 6. _suite_converter.structure (generic fallback)

        # If we use a list and the type is Union[int, str], it might fail in cattrs
        # but structure_every_union raises TypeError if it falls through.
        # However, cattrs might raise an exception inside the loop, which is caught and rejected.
        structure_every_union([], Union[int, Decimal])


def test_initialise_converter_idempotent():
    from tested.parsing import initialise_converter

    # Should not raise any errors when called multiple times
    initialise_converter()
    initialise_converter()


def test_parse_json_value():
    from tested.datatypes import BasicStringTypes
    from tested.parsing import parse_json_value
    from tested.serialisation import StringType

    json_str = '{"type": "text", "data": "hello"}'
    result = parse_json_value(json_str)
    assert isinstance(result, StringType)
    assert result.type == BasicStringTypes.TEXT
    assert result.data == "hello"


def test_parse_json_suite_and_to_json():
    from tested.parsing import parse_json_suite, suite_to_json
    from tested.testsuite import Suite

    # A very minimal suite
    json_str = '{"tabs": [], "namespace": "test_namespace"}'
    suite = parse_json_suite(json_str)
    assert isinstance(suite, Suite)
    assert suite.tabs == []
    assert suite.namespace == "test_namespace"

    # Convert back to json
    back_to_json = suite_to_json(suite)
    # The default Suite might have other fields, but namespace should be there if not default
    assert "test_namespace" in back_to_json
