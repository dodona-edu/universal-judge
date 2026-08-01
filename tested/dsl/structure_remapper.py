from tested.dsl.yaml_types import YamlObject

_ROOT = {"units": "tabs"}
_TAB = {"unit": "tab", "cases": "contexts", "scripts": "testcases"}
_CONTEXT = {"script": "testcases"}


def _rename_fields(value: dict, mapping: dict[str, str]):
    for old, new in mapping.items():
        if old in value:
            value[new] = value.pop(old)


def _convert_contexts(context_list: YamlObject):
    if isinstance(context_list, list):
        for context in context_list:
            if isinstance(context, dict):
                _rename_fields(context, _CONTEXT)


def _convert_tabs(tab_list: YamlObject):
    if isinstance(tab_list, list):
        for tab in tab_list:
            if not isinstance(tab, dict):
                continue

            had_cases = "cases" in tab
            _rename_fields(tab, _TAB)
            if had_cases:
                _convert_contexts(tab["contexts"])


def _convert_dsl(dsl_object: YamlObject):
    if isinstance(dsl_object, dict):
        had_units = "units" in dsl_object
        _rename_fields(dsl_object, _ROOT)
        if had_units:
            _convert_tabs(dsl_object["tabs"])
    elif isinstance(dsl_object, list):
        _convert_tabs(dsl_object)

    return dsl_object


def remap_structure(dsl_data: YamlObject):
    """
    Remaps the deprecated and not that widely used alternative structure of the DSL test suites
    to the common format.

    Note that this function does not validate anything. If the DSL is not in the expected format,
    it aborts and relies on the validation to catch the error.
    """

    # First, check if we actually need to convert. Mixing formats is not allowed,
    # so we can just check the first things.
    if isinstance(dsl_data, dict) and "units" not in dsl_data:
        return
    if (
        isinstance(dsl_data, list)
        and len(dsl_data) > 0
        and isinstance(dsl_data[0], dict)
        and "unit" not in dsl_data[0]
    ):
        return

    # Renaming the things in-place.
    _convert_dsl(dsl_data)
