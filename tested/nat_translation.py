import re
import sys
from typing import cast

import yaml

from tested.dsl.translate_parser import (
    ExpressionString,
    NaturalLanguageMap,
    ReturnOracle,
    YamlDict,
    YamlObject,
    _parse_yaml,
    _validate_dsl,
    _validate_testcase_combinations,
)


def parse_value(value: list | str | int | float | dict, flattened_stack: dict) -> list | str | int | float | dict:
    if isinstance(value, str):
        return format_string(value, flattened_stack)
    elif isinstance(value, dict):
        return {k: parse_value(v, flattened_stack) for k, v in value.items()}
    elif isinstance(value, list):
        return [parse_value(v, flattened_stack) for v in value]

    return value


def get_replacement(language: str, translation_stack: list, match: re.Match) -> str:
    word = match.group(1)
    current = -1
    stack = translation_stack[current]
    while abs(current) <= len(translation_stack) and word not in stack:
        current -= 1
        stack = translation_stack[current]
    if abs(current) <= len(translation_stack):
        translations = stack[word]
        assert language in translations
        word = translations[language]

    return word


def flatten_stack(translation_stack: list, language: str) -> dict:
    flattened = {}
    for d in translation_stack:

        flattened.update({k: v[language] for k, v in d.items() if language in v})
    return flattened


def format_string(string: str, flattened) -> str:
    return string.format(**flattened)


def translate_io(
    io_object: YamlObject, key: str, language: str, flat_stack: dict
) -> YamlObject:
    if isinstance(io_object, NaturalLanguageMap):
        assert language in io_object
        io_object = io_object[language]
    if isinstance(io_object, dict):
        data = io_object[key]
        if isinstance(data, dict):
            assert language in data
            data = data[language]
        assert isinstance(data, str)
        io_object[key] = format_string(data, flat_stack)

    # Perform translation based of translation stack.
    print(io_object)
    if isinstance(io_object, str):
        return format_string(io_object, flat_stack)

    return io_object


def translate_testcase(
    testcase: YamlDict, language: str, translation_stack: list
) -> YamlDict:
    _validate_testcase_combinations(testcase)
    flat_stack = flatten_stack(translation_stack, language)

    key_to_set = "statement" if "statement" in testcase else "expression"
    if (expr_stmt := testcase.get(key_to_set)) is not None:
        # Must use !natural_language
        if isinstance(expr_stmt, NaturalLanguageMap):
            assert language in expr_stmt
            expr_stmt = expr_stmt[language]

        # Perform translation based of translation stack.
        if isinstance(expr_stmt, dict):
            testcase[key_to_set] = {
                k: format_string(cast(str, v), flat_stack) for k, v in expr_stmt.items()
            }
        elif isinstance(expr_stmt, str):
            testcase[key_to_set] = format_string(expr_stmt, flat_stack)

    else:
        if (stdin_stmt := testcase.get("stdin")) is not None:
            if isinstance(stdin_stmt, dict):
                assert language in stdin_stmt
                stdin_stmt = stdin_stmt[language]

            # Perform translation based of translation stack.
            assert isinstance(stdin_stmt, str)
            testcase["stdin"] = format_string(stdin_stmt, flat_stack)

        arguments = testcase.get("arguments", [])
        if isinstance(arguments, dict):
            assert language in arguments
            arguments = arguments[language]

        # Perform translation based of translation stack.
        assert isinstance(arguments, list)
        testcase["arguments"] = [
            format_string(str(arg), flat_stack) for arg in arguments
        ]

    if (stdout := testcase.get("stdout")) is not None:
        # Must use !natural_language
        testcase["stdout"] = translate_io(stdout, "data", language, flat_stack)

    if (file := testcase.get("file")) is not None:
        # Must use !natural_language
        if isinstance(file, NaturalLanguageMap):
            assert language in file
            testcase["file"] = file[language]
        # TODO: SHOULD I ADD SUPPORT FOR TRANSLATION STACK HERE?
    if (stderr := testcase.get("stderr")) is not None:
        testcase["stderr"] = translate_io(stderr, "data", language, flat_stack)

    if (exception := testcase.get("exception")) is not None:
        testcase["exception"] = translate_io(exception, "message", language, flat_stack)

    if (result := testcase.get("return")) is not None:
        if isinstance(result, ReturnOracle):
            arguments = result.get("arguments", [])
            if isinstance(arguments, dict):
                assert language in arguments
                arguments = arguments[language]

            # Perform translation based of translation stack.
            result["arguments"] = [
                format_string(str(arg), flat_stack) for arg in arguments
            ]

            value = result.get("value")
            # Must use !natural_language
            if isinstance(value, NaturalLanguageMap):
                assert language in value
                value = value[language]

            assert isinstance(value, str)
            result["value"] = parse_value(value, flat_stack)
            testcase["return"] = result

        elif isinstance(result, NaturalLanguageMap):
            # Must use !natural_language
            assert language in result
            testcase["return"] = parse_value(result[language], flat_stack)
        elif result is not None:
            testcase["return"] = parse_value(result, flat_stack)

    if (description := testcase.get("description")) is not None:
        # Must use !natural_language
        if isinstance(description, NaturalLanguageMap):
            assert language in description
            description = description[language]

        if isinstance(description, str):
            testcase["description"] = format_string(description, flat_stack)

        if isinstance(description, dict):
            dd = description["description"]
            if isinstance(dd, dict):
                assert language in dd
                dd = dd[language]

            assert isinstance(dd, str)
            description["description"] = format_string(dd, flat_stack)

        testcase["description"] = description

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
        if "translation" in context:
            translation_stack.append(context["translation"])

        key_to_set = "script" if "script" in context else "testcases"
        raw_testcases = context.get(key_to_set)
        assert isinstance(raw_testcases, list)
        context[key_to_set] = translate_testcases(
            raw_testcases, language, translation_stack
        )
        if "files" in context:
            files = context.get("files")
            if isinstance(files, NaturalLanguageMap):
                assert language in files
                context["files"] = files[language]
        result.append(context)
        if "translation" in context:
            translation_stack.pop()
            context.pop("translation")

    return result


def translate_tab(tab: YamlDict, language: str, translation_stack: list) -> YamlDict:
    key_to_set = "unit" if "unit" in tab else "tab"
    name = tab.get(key_to_set)

    if isinstance(name, dict):
        assert language in name
        name = name[language]

    assert isinstance(name, str)
    tab[key_to_set] = format_string(name, flatten_stack(translation_stack, language))

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
    translated_dsl = translate_dsl(new_yaml, lang)
    yaml_string = convert_to_yaml(translated_dsl)
    print(yaml_string)
    _validate_dsl(_parse_yaml(yaml_string))
