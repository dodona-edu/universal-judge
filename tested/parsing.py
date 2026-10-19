"""
Utilities to parse JSON into the data we have.

Since we need to set up quite a few converters, we re-use
a single converter for all, both the serialization and suite.
"""

import logging
from decimal import Decimal
from typing import TYPE_CHECKING, Any, Callable, TypeVar, cast

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

T = TypeVar("T")

# Saves the registry of structure hooks.
# Note that this assumes we only have a single converter.
_TESTED_STRUCTURE_HOOK_REGISTRIES: dict[type, Callable[[dict[str, Any], type], Any]] = (
    dict()
)


def structure_decimal(obj: Any, _) -> Decimal:
    return Decimal(str(obj))


def unstructure_decimal(obj: Decimal) -> str:
    return str(obj)


def structure_every_union(to_convert: Any, the_type: Any) -> Any:
    from tested.serialisation import Identifier
    from tested.testsuite import ContentPath

    _logger.debug("=== Finding type for %s, from %s...", to_convert, the_type)
    if to_convert is None and type(None) in get_args(the_type):
        _logger.debug("Yes: found None.")
        return None
    if isinstance(to_convert, bool) and bool in get_args(the_type):
        _logger.debug("Yes: found boolean: %s.", to_convert)
        return to_convert
    if isinstance(to_convert, ContentPath) and ContentPath in get_args(the_type):
        _logger.debug(f"Yes: found content path: {to_convert}.")
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
                assert isinstance(to_convert, (str, dict))
            result = _suite_converter.structure(to_convert, possible_class)
            _logger.debug("%s accepted.", debug_message)
            return result
        except Exception:
            _logger.debug("%s rejected.", debug_message)
    raise TypeError(
        f"{to_convert} could not be converted into a {the_type}. Check the syntax or file a bug."
    )


_identifier_from_dict: Callable[[dict[str, Any], type], Any] | None = None


def structure_identifier(obj: Any, cl: type) -> Any:
    """
    Structure an identifier, which is either a plain string (a variable in the test
    suite) or a dict with a name and a scope.
    """
    from tested.serialisation import Identifier, Scope, ScopeType

    if isinstance(obj, str):
        return Identifier(name=obj)
    if isinstance(obj, dict) and obj.get("type") == "property":
        # Legacy: properties were function calls of type "property". Without a
        # namespace, it is a global of the submission; otherwise, the namespace
        # is the receiver of the property.
        namespace = obj.get("namespace")
        if namespace in (None, ""):
            return Identifier(name=obj["name"], scope=ScopeType.SUBMISSION)
        return Identifier(
            name=obj["name"],
            scope=_suite_converter.structure(namespace, Scope),  # pyright: ignore
        )
    if isinstance(obj, dict):
        # Generated lazily, since it needs the hooks for the fields to be registered.
        global _identifier_from_dict
        if _identifier_from_dict is None:
            _identifier_from_dict = make_dict_structure_fn(Identifier, _suite_converter)
        return _identifier_from_dict(obj, cl)
    raise TypeError(f"An identifier must be a string or an object, got {obj!r}.")


def _legacy_namespace(d: dict[str, Any], cl: type, next_hook):
    """
    Legacy: function calls used to have a "namespace" instead of a "scope". A missing,
    null or empty namespace meant a function from the submission, which is now the
    default scope.
    """
    if "namespace" in d:
        if "scope" in d:
            raise ValueError("You cannot use 'namespace' and 'scope' simultaneously.")
        namespace = d.pop("namespace")
        if namespace not in (None, ""):
            if d.get("type") == "constructor":
                raise ValueError("Constructors with a namespace are not supported.")
            d["scope"] = namespace
    return next_hook(d, cl)


def _legacy_property(d: dict[str, Any], cl: type, next_hook):
    """
    Legacy: property assignments used to have a "property" (a function call of type
    "property") instead of a "target". The property itself is converted by
    `structure_identifier`.
    """
    if "property" in d:
        if "target" in d:
            raise ValueError("You cannot use 'property' and 'target' simultaneously.")
        prop = d.pop("property")
        if not isinstance(prop, dict) or prop.get("type") != "property":
            raise ValueError(f"Assigning requires a property, got {prop!r}.")
        if prop.get("namespace") in (None, ""):
            raise ValueError(
                "Assigning to a global of the submission is not supported."
            )
        d["target"] = prop
    return next_hook(d, cl)


def initialise_converter():
    from tested.serialisation import FunctionCall, Identifier, PropertyAssignment

    global initialized
    if initialized:
        return
    initialized = True

    _suite_converter.register_structure_hook(Decimal, structure_decimal)
    _suite_converter.register_unstructure_hook(Decimal, unstructure_decimal)
    _suite_converter.register_structure_hook(Identifier, structure_identifier)
    _suite_converter.register_structure_hook_factory(
        lambda t: bool(is_union_type(t)), lambda _: structure_every_union
    )
    # Legacy formats; registered last, since generating these hooks needs the hooks
    # above (e.g. for the unions in the fields).
    _chain_structure_hook(FunctionCall, _legacy_namespace)
    _chain_structure_hook(PropertyAssignment, _legacy_property)


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


def _chain_structure_hook(
    cls: type[T],
    wrapper: Callable[
        [dict[str, Any], type[T], Callable[[dict[str, Any], type[T]], Any]], Any
    ],
):
    """
    Compose structure hooks for a class.

    cattrs only keeps one structure hook per class; decorator stacking would otherwise
    overwrite earlier hooks. We keep our own per-converter registry and wrap the
    previously registered hook.

    Since the wrapper is executed first and then the previous one is called, the final
    order in which the transformations are applied is top to bottom.

    For example:
        @fallback_field(..., 1)   # top
        @fallback_field(..., 2)
        @ignore_field(..., "show_expected")
        @ignore_field(..., "type")    # bottom
        class TextOutputChannel(...):

    When structuring a dictionary, the transformations will be applied in the same order:

    1. `@fallback_field(..., 1)`
    2. `@fallback_field(..., 2)`
    3. `@ignore_field(..., "show_expected")`
    4. `@ignore_field(..., "type")`
    """
    converter = get_converter()

    previous = _TESTED_STRUCTURE_HOOK_REGISTRIES.get(cls)
    if previous is None:
        previous = make_dict_structure_fn(
            cls, converter, _cattrs_forbid_extra_keys=False
        )

    def composed(d: dict[str, Any], cl: type[T]):
        if not isinstance(d, dict):
            return previous(d, cl)

        # Work on a shallow copy so we don't mutate dicts.
        d2 = dict(d)
        return wrapper(d2, cl, previous)

    # Python's types cannot encode dict[T, Callable[..., T]].
    # However, we know that the key is a type that the value expects, so cast it.
    _TESTED_STRUCTURE_HOOK_REGISTRIES[cls] = cast(
        Callable[[dict[str, Any], type], Any], composed
    )

    converter.register_structure_hook(cls, composed)


def ignore_field(*fields: str):
    def decorator(cls):
        def _wrapper(d: dict[str, Any], cl: type, next_hook):
            for to_ignore in fields:
                d.pop(to_ignore, None)
            return next_hook(d, cl)

        _chain_structure_hook(cls, _wrapper)
        return cls

    return decorator


def fallback_field(
    old_to_new_field: dict[str, str | tuple[str, Callable[[Any, dict[str, Any]], Any]]],
):
    def decorator(cls):
        def _wrapper(d: dict[str, Any], cl: type, next_hook):
            for old_name, mapping in old_to_new_field.items():
                if old_name in d:
                    if isinstance(mapping, tuple):
                        new_name, mapper = mapping
                        if new_name in d:
                            raise ValueError(
                                f"You cannot use {new_name} and {old_name} simultaneously. "
                                f"Migrate to {new_name}."
                            )
                        d[new_name] = mapper(d[old_name], d)
                    else:
                        new_name = mapping
                        if new_name not in d:
                            d[new_name] = d[old_name]
            return next_hook(d, cl)

        _chain_structure_hook(cls, _wrapper)
        return cls

    return decorator


def get_converter() -> JsonConverter:
    initialise_converter()
    return _suite_converter
