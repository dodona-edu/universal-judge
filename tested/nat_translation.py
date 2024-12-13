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

def flatten_stack(translation_stack: list) -> dict:
    flattened = {}
    for d in reversed(translation_stack):
        flattened.update(d)
    return flattened

def format_string(string: str, flattened) -> str:
    return string.format(**flattened)


def translate_testcase(
    testcase: YamlDict, language: str, translation_stack: list
) -> YamlDict:
    _validate_testcase_combinations(testcase)
    flat_stack = flatten_stack(translation_stack)

    key_to_set = "statement" if "statement" in testcase else "expression"
    if (expr_stmt := testcase.get(key_to_set)) is not None:
        # Must use !natural_language
        if isinstance(expr_stmt, NaturalLanguageMap):
            assert language in expr_stmt
            testcase[key_to_set] = expr_stmt[language]

        # Perform translation based of translation stack.
        expr_stmt = testcase.get(key_to_set)
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
                testcase["stdin"] = stdin_stmt[language]

            # Perform translation based of translation stack.
            testcase["stdin"] = format_string(testcase["stdin"], flat_stack)

        arguments = testcase.get("arguments", [])
        if isinstance(arguments, dict):
            assert language in arguments
            testcase["arguments"] = arguments[language]

        # Perform translation based of translation stack.
        testcase["arguments"] = format_string(testcase["arguments"], flat_stack)

    if (stdout := testcase.get("stdout")) is not None:
        # Must use !natural_language
        if isinstance(stdout, NaturalLanguageMap):
            assert language in stdout
            testcase["stdout"] = stdout[language]
        elif isinstance(stdout, dict):
            data = stdout["data"]
            if isinstance(data, dict):
                assert language in data
                stdout["data"] = data[language]
                testcase["stdout"] = stdout
    if (file := testcase.get("file")) is not None:
        # Must use !natural_language
        if isinstance(file, NaturalLanguageMap):
            assert language in file
            testcase["file"] = file[language]
    if (stderr := testcase.get("stderr")) is not None:
        # Must use !natural_language
        if isinstance(stderr, NaturalLanguageMap):
            assert language in stderr
            testcase["stderr"] = stderr[language]
        elif isinstance(stderr, dict):
            data = stderr["data"]
            if isinstance(data, dict):
                assert language in data
                stderr["data"] = data[language]
                testcase["stderr"] = stderr

    if (exception := testcase.get("exception")) is not None:
        if isinstance(exception, NaturalLanguageMap):
            assert language in exception
            testcase["exception"] = exception[language]
        elif isinstance(exception, dict):
            message = exception["message"]
            if isinstance(message, dict):
                assert language in message
                exception["message"] = message[language]
                testcase["exception"] = exception

    if (result := testcase.get("return")) is not None:
        if isinstance(result, ReturnOracle):
            arguments = result.get("arguments", [])
            if isinstance(arguments, dict):
                assert language in arguments
                result["arguments"] = arguments[language]

            value = result.get("value")
            # Must use !natural_language
            if isinstance(value, NaturalLanguageMap):
                assert language in value
                result["value"] = value[language]

            testcase["return"] = result
        elif isinstance(result, NaturalLanguageMap):
            # Must use !natural_language
            assert language in result
            testcase["return"] = result[language]

    if (description := testcase.get("description")) is not None:
        # Must use !natural_language
        if isinstance(description, NaturalLanguageMap):
            assert language in description
            testcase["description"] = description[language]
        elif isinstance(description, dict):
            dd = description["description"]
            if isinstance(dd, dict):
                assert language in dd
                description["description"] = dd[language]
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
            context.pop("translation")
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

    return result


def translate_tab(tab: YamlDict, language: str, translation_stack: list) -> YamlDict:
    key_to_set = "unit" if "unit" in tab else "tab"
    name = tab.get(key_to_set)

    if isinstance(name, dict):
        assert language in name
        tab[key_to_set] = name[language]

    tab[key_to_set] = format_string(tab[key_to_set], flatten_stack(translation_stack))

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
            tab.pop("translation")
        result.append(translate_tab(tab, language, translation_stack))
        if "translation" in tab:
            translation_stack.pop()

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
