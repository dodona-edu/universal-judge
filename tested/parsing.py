"""
Utilities to parse JSON into the data we have.

Since we need to set up quite a few converters, we re-use
a single converter for all, both the serialization and suite.
"""

import logging
from collections.abc import Callable
from decimal import Decimal
from typing import TYPE_CHECKING, Any

from cattrs import Converter
from cattrs.gen import make_dict_structure_fn
from cattrs.preconf.json import JsonConverter, make_converter
from typing_inspect import is_union_type

from tested.utils import get_args

if TYPE_CHECKING:
    from tested.serialisation import Value
    from tested.testsuite import Suite

_logger = logging.getLogger(__name__)

_suite_converter = make_converter(forbid_extra_keys=True, omit_if_default=True)
initialized = False


def structure_decimal(obj: Any, _) -> Decimal:
    return Decimal(str(obj))


def unstructure_decimal(obj: Decimal) -> str:
    return str(obj)


def structure_every_union(to_convert: Any, the_type: type) -> Any:
    from tested.serialisation import Identifier

    _logger.debug("=== Finding type for %s, from %s...", to_convert, the_type)
    if to_convert is None and type(None) in get_args(the_type):
        _logger.debug("Yes: found None.")
        return None
    if isinstance(to_convert, bool) and bool in get_args(the_type):
        _logger.debug("Yes: found boolean: %s.", to_convert)
        return to_convert

    for possible_class in get_args(the_type):
        debug_message = f"{possible_class} -> "
        # noinspection PyBroadException
        try:
            if isinstance(to_convert, int):
                assert possible_class is int
            if possible_class is int:
                assert isinstance(to_convert, int)
            if isinstance(to_convert, float):
                assert possible_class is float
            if possible_class is float:
                assert isinstance(to_convert, float)
            if possible_class is Identifier:
                assert isinstance(to_convert, str)
            result = _suite_converter.structure(to_convert, possible_class)
            _logger.debug("%s accepted.", debug_message)
            return result
        except Exception:
            _logger.debug("%s rejected.", debug_message)
    raise TypeError(
        f"{to_convert} could not be converted into a {the_type}. Check the syntax or file a bug."
    )


def initialise_converter():
    global initialized
    if initialized:
        return
    initialized = True

    _suite_converter.register_structure_hook(Decimal, structure_decimal)
    _suite_converter.register_unstructure_hook(Decimal, unstructure_decimal)
    _suite_converter.register_structure_hook_factory(
        lambda t: bool(is_union_type(t)), lambda _: structure_every_union
    )


def parse_json_value(value: str) -> "Value":
    """
    Parse the json of a value into the relevant data structures.

    If ``value`` is not valid json, a `SerialisationError` will be thrown.

    :param value: The json to be parsed.
    :return: The parsed data.
    """
    initialise_converter()
    from tested.serialisation import Value

    return _suite_converter.loads(value, Value)  # pyright: ignore


def parse_json_suite(value: str) -> "Suite":
    """Parse a test suite into the structures."""
    initialise_converter()
    from tested.testsuite import Suite

    return _suite_converter.loads(value, Suite)


def suite_to_json(suite: "Suite") -> str:
    initialise_converter()
    return _suite_converter.dumps(suite)


def fallback_field(converter_arg: Converter, old_to_new_field: dict[str, str]):
    def decorator(cls):
        struct = make_dict_structure_fn(
            cls, converter_arg, _cattrs_forbid_extra_keys=False
        )

        def structure(d, cl):
            for k, v in old_to_new_field.items():
                if k in d:
                    d[v] = d[k]

            return struct(d, cl)

        converter_arg.register_structure_hook(cls, structure)

        return cls

    return decorator


def custom_fallback_field(
    converter_arg: Converter,
    old_to_new_field: dict[str, tuple[str, Callable[[Any], Any]]],
):
    def decorator(cls):
        struct = make_dict_structure_fn(
            cls, converter_arg, _cattrs_forbid_extra_keys=False
        )

        def structure(d, cl):
            for k, (new_name, mapper) in old_to_new_field.items():
                if k in d:
                    if new_name in d:
                        raise ValueError(
                            f"You cannot use {new_name} and {k} simultaneously. Migrate to {new_name}."
                        )
                    d[new_name] = mapper(d[k])

            return struct(d, cl)

        converter_arg.register_structure_hook(cls, structure)

        return cls

    return decorator


def ignore_field(converter_arg: Converter, *fields: str):
    def decorator(cls):
        struct = make_dict_structure_fn(
            cls, converter_arg, _cattrs_forbid_extra_keys=False
        )

        def structure(d, cl):
            for to_ignore in fields:
                if to_ignore in d:
                    del d[to_ignore]

            return struct(d, cl)

        converter_arg.register_structure_hook(cls, structure)

        return cls

    return decorator


def get_converter() -> JsonConverter:
    initialise_converter()
    return _suite_converter
