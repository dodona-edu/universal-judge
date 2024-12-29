import sys

import yaml

from tested.dsl.translate_parser import (
    DslValidationError,
    ExpressionString,
    NaturalLanguageMap,
    ProgrammingLanguageMap,
    ReturnOracle,
    YamlDict,
    YamlObject,
    _parse_yaml,
    _validate_dsl,
    _validate_testcase_combinations,
    convert_validation_error_to_group,
    load_schema_validator,
)


def validate_pre_dsl(dsl_object: YamlObject):
    """
    Validate a DSl object.

    :param dsl_object: The object to validate.
    :return: True if valid, False otherwise.
    """
    _SCHEMA_VALIDATOR = load_schema_validator("schema-strict-nat-translation.json")
    errors = list(_SCHEMA_VALIDATOR.iter_errors(dsl_object))
    if len(errors) == 1:
        message = (
            "Validating the DSL resulted in an error. "
            "The most specific sub-exception is often the culprit. "
        )
        error = convert_validation_error_to_group(errors[0])
        if isinstance(error, ExceptionGroup):
            raise ExceptionGroup(message, error.exceptions)
        else:
            raise DslValidationError(message + str(error)) from error
    elif len(errors) > 1:
        the_errors = [convert_validation_error_to_group(e) for e in errors]
        message = "Validating the DSL resulted in some errors."
        raise ExceptionGroup(message, the_errors)


def natural_langauge_map_translation(value: YamlObject, language: str):
    if isinstance(value, NaturalLanguageMap):
        assert language in value
        value = value[language]
    return value


def translate_input_files(
    dsl_object: dict, language: str, flattened_stack: dict
) -> dict:
    if (files := dsl_object.get("files")) is not None:
        # Translation map can happen at the top level.
        files = natural_langauge_map_translation(files, language)
        assert isinstance(files, list)
        for i in range(len(files)):
            file = files[i]

            # Do the formatting.
            if isinstance(file, dict):
                name = file["name"]
                assert isinstance(name, str)
                file["name"] = format_string(name, flattened_stack)
                url = file["url"]
                assert isinstance(url, str)
                file["url"] = format_string(url, flattened_stack)
            files[i] = file

        dsl_object["files"] = files
    return dsl_object


def parse_value(value: YamlObject, flattened_stack: dict) -> YamlObject:

    # Will format the strings in different values.
    if isinstance(value, str):
        return format_string(value, flattened_stack)
    elif isinstance(value, dict):
        return {k: parse_value(v, flattened_stack) for k, v in value.items()}
    elif isinstance(value, list):
        return [parse_value(v, flattened_stack) for v in value]

    return value


def flatten_stack(translation_stack: list, language: str) -> dict:
    # Will transform a list of translation maps into a dict that
    # has all the keys defined over all the different translation map and will have
    # the value of the newest definition. In this definition we also chose
    # the translation of the provided language.
    flattened = {}
    for d in translation_stack:
        flattened.update({k: v[language] for k, v in d.items() if language in v})
    return flattened


def format_string(string: str, flattened) -> str:
    return string.format(**flattened)


def translate_io(
    io_object: YamlObject, key: str, language: str, flat_stack: dict
) -> YamlObject:
    # Translate NaturalLanguageMap
    io_object = natural_langauge_map_translation(io_object, language)

    if isinstance(io_object, dict):
        data = natural_langauge_map_translation(io_object[key], language)
        io_object[key] = parse_value(data, flat_stack)

    # Perform translation based of translation stack.
    elif isinstance(io_object, str):
        return format_string(io_object, flat_stack)

    return io_object


def translate_testcase(
    testcase: YamlDict, language: str, translation_stack: list
) -> YamlDict:
    _validate_testcase_combinations(testcase)
    flat_stack = flatten_stack(translation_stack, language)

    key_to_set = "statement" if "statement" in testcase else "expression"
    if (expr_stmt := testcase.get(key_to_set)) is not None:
        # Program language translation found
        if isinstance(expr_stmt, ProgrammingLanguageMap):
            expr_stmt = {
                k: natural_langauge_map_translation(v, language)
                for k, v in expr_stmt.items()
            }
        elif isinstance(
            expr_stmt, NaturalLanguageMap
        ):  # Natural language translation found
            assert language in expr_stmt
            expr_stmt = expr_stmt[language]

        testcase[key_to_set] = parse_value(expr_stmt, flat_stack)
    else:
        if (stdin_stmt := testcase.get("stdin")) is not None:
            # Translate NaturalLanguageMap
            stdin_stmt = natural_langauge_map_translation(stdin_stmt, language)

            # Perform translation based of translation stack.
            assert isinstance(stdin_stmt, str)
            testcase["stdin"] = format_string(stdin_stmt, flat_stack)

        # Translate NaturalLanguageMap
        arguments = testcase.get("arguments", [])
        arguments = natural_langauge_map_translation(arguments, language)

        # Perform translation based of translation stack.
        assert isinstance(arguments, list)
        testcase["arguments"] = parse_value(arguments, flat_stack)

    if (stdout := testcase.get("stdout")) is not None:
        testcase["stdout"] = translate_io(stdout, "data", language, flat_stack)

    if (file := testcase.get("file")) is not None:
        # Translate NaturalLanguageMap
        file = natural_langauge_map_translation(file, language)

        assert isinstance(file, dict)
        file["content"] = format_string(str(file["content"]), flat_stack)
        file["location"] = format_string(str(file["location"]), flat_stack)

        testcase["file"] = file

    if (stderr := testcase.get("stderr")) is not None:
        testcase["stderr"] = translate_io(stderr, "data", language, flat_stack)

    if (exception := testcase.get("exception")) is not None:
        testcase["exception"] = translate_io(exception, "message", language, flat_stack)

    if (result := testcase.get("return")) is not None:
        if isinstance(result, ReturnOracle):
            arguments = result.get("arguments", [])
            arguments = natural_langauge_map_translation(arguments, language)

            # Perform translation based of translation stack.
            result["arguments"] = parse_value(arguments, flat_stack)

            value = result.get("value")
            value = natural_langauge_map_translation(value, language)

            result["value"] = parse_value(value, flat_stack)
            testcase["return"] = result

        elif isinstance(result, NaturalLanguageMap):
            assert language in result
            testcase["return"] = parse_value(result[language], flat_stack)
        elif result is not None:
            testcase["return"] = parse_value(result, flat_stack)

    if (description := testcase.get("description")) is not None:
        description = natural_langauge_map_translation(description, language)

        if isinstance(description, str):
            testcase["description"] = format_string(description, flat_stack)
        else:
            assert isinstance(description, dict)
            dd = description["description"]
            dd = natural_langauge_map_translation(dd, language)

            assert isinstance(dd, str)
            description["description"] = format_string(dd, flat_stack)

    testcase = translate_input_files(testcase, language, flat_stack)

    return testcase


def translate_testcases(
    testcases: list, language: str, translation_stack: list
) -> list:
    result = []
    for testcase in testcases:
        assert isinstance(testcase, dict)
        result.append(translate_testcase(testcase, language, translation_stack))

    return result


def translate_contexts(contexts: list, language: str, translation_stack: list) -> list:
    result = []
    for context in contexts:
        assert isinstance(context, dict)

        # Add translation to stack
        if "translation" in context:
            translation_stack.append(context["translation"])

        key_to_set = "script" if "script" in context else "testcases"
        raw_testcases = context.get(key_to_set)
        assert isinstance(raw_testcases, list)
        context[key_to_set] = translate_testcases(
            raw_testcases, language, translation_stack
        )

        flat_stack = flatten_stack(translation_stack, language)
        context = translate_input_files(context, language, flat_stack)
        result.append(context)

        # Pop translation from stack
        if "translation" in context:
            translation_stack.pop()
            context.pop("translation")

    return result


def translate_tab(tab: YamlDict, language: str, translation_stack: list) -> YamlDict:
    key_to_set = "unit" if "unit" in tab else "tab"
    name = tab.get(key_to_set)
    name = natural_langauge_map_translation(name, language)

    assert isinstance(name, str)
    flat_stack = flatten_stack(translation_stack, language)
    tab[key_to_set] = format_string(name, flat_stack)

    tab = translate_input_files(tab, language, flat_stack)

    # The tab can have testcases or contexts.
    if "contexts" in tab:
        assert isinstance(tab["contexts"], list)
        tab["contexts"] = translate_contexts(
            tab["contexts"], language, translation_stack
        )
    elif "cases" in tab:
        assert "unit" in tab
        # We have testcases N.S. / contexts O.S.
        assert isinstance(tab["cases"], list)
        tab["cases"] = translate_contexts(tab["cases"], language, translation_stack)
    elif "testcases" in tab:
        # We have scripts N.S. / testcases O.S.
        assert "tab" in tab
        assert isinstance(tab["testcases"], list)
        tab["testcases"] = translate_testcases(
            tab["testcases"], language, translation_stack
        )
    else:
        assert "scripts" in tab
        assert isinstance(tab["scripts"], list)
        tab["scripts"] = translate_testcases(
            tab["scripts"], language, translation_stack
        )
    return tab


def translate_tabs(dsl_list: list, language: str, translation_stack=None) -> list:
    if translation_stack is None:
        translation_stack = []

    result = []
    for tab in dsl_list:
        assert isinstance(tab, dict)

        if "translation" in tab:
            translation_stack.append(tab["translation"])

        result.append(translate_tab(tab, language, translation_stack))
        if "translation" in tab:
            translation_stack.pop()
            tab.pop("translation")

    return result


def translate_dsl(dsl_object: YamlObject, language: str) -> YamlObject:
    if isinstance(dsl_object, list):
        return translate_tabs(dsl_object, language)
    else:
        assert isinstance(dsl_object, dict)
        key_to_set = "units" if "units" in dsl_object else "tabs"
        tab_list = dsl_object.get(key_to_set)
        assert isinstance(tab_list, list)
        translation_stack = []
        if "translation" in dsl_object:
            translation_stack.append(dsl_object["translation"])
            dsl_object.pop("translation")

        flat_stack = flatten_stack(translation_stack, language)
        dsl_object = translate_input_files(dsl_object, language, flat_stack)
        dsl_object[key_to_set] = translate_tabs(tab_list, language, translation_stack)
        return dsl_object


def parse_yaml(yaml_path: str) -> YamlObject:
    with open(yaml_path, "r") as stream:
        result = _parse_yaml(stream.read())

    return result


def convert_to_yaml(yaml_object: YamlObject) -> str:
    def oracle_representer(dumper, data):
        return dumper.represent_mapping("!oracle", data)

    def expression_representer(dumper, data):
        return dumper.represent_scalar("!expression", data)

    # Register the representer for the ReturnOracle object
    yaml.add_representer(ReturnOracle, oracle_representer)
    yaml.add_representer(ExpressionString, expression_representer)
    return yaml.dump(yaml_object, sort_keys=False)


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected atleast two argument (path to yaml file and language)."

    path = sys.argv[1]
    lang = sys.argv[2]
    new_yaml = parse_yaml(path)
    validate_pre_dsl(new_yaml)
    translated_dsl = translate_dsl(new_yaml, lang)
    yaml_string = convert_to_yaml(translated_dsl)
    print(yaml_string)
    _validate_dsl(_parse_yaml(yaml_string))
