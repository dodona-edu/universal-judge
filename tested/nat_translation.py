import sys

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


def translate_testcase(testcase: YamlDict, language: str) -> YamlDict:
    _validate_testcase_combinations(testcase)

    key_to_set = "statement" if "statement" in testcase else "expression"
    if (expr_stmt := testcase.get(key_to_set)) is not None:
        # Must use !natural_language
        if isinstance(expr_stmt, NaturalLanguageMap):
            assert language in expr_stmt
            testcase[key_to_set] = expr_stmt[language]
    else:
        if (stdin_stmt := testcase.get("stdin")) is not None:
            if isinstance(stdin_stmt, dict):
                assert language in stdin_stmt
                testcase["stdin"] = stdin_stmt[language]

        arguments = testcase.get("arguments", [])
        if isinstance(arguments, dict):
            assert language in arguments
            testcase["arguments"] = arguments[language]

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


def translate_testcases(testcases: list, language: str) -> list:
    result = []
    for testcase in testcases:
        assert isinstance(testcase, dict)
        result.append(translate_testcase(testcase, language))

    return result


def translate_contexts(contexts: list, language: str) -> list:
    result = []
    for context in contexts:
        assert isinstance(context, dict)
        key_to_set = "script" if "script" in context else "testcases"
        raw_testcases = context.get(key_to_set)
        assert isinstance(raw_testcases, list)
        context[key_to_set] = translate_testcases(raw_testcases, language)
        if "files" in context:
            files = context.get("files")
            if isinstance(files, NaturalLanguageMap):
                assert language in files
                context["files"] = files[language]
        result.append(context)

    return result


def translate_tab(tab: YamlDict, language: str) -> YamlDict:
    key_to_set = "unit" if "unit" in tab else "tab"
    name = tab.get(key_to_set)

    if isinstance(name, dict):
        assert language in name
        tab[key_to_set] = name[language]

    # The tab can have testcases or contexts.
    if "contexts" in tab:
        assert isinstance(tab["contexts"], list)
        tab["contexts"] = translate_contexts(tab["contexts"], language)
    elif "cases" in tab:
        assert "unit" in tab
        # We have testcases N.S. / contexts O.S.
        assert isinstance(tab["cases"], list)
        tab["cases"] = translate_contexts(tab["cases"], language)
    elif "testcases" in tab:
        # We have scripts N.S. / testcases O.S.
        assert "tab" in tab
        assert isinstance(tab["testcases"], list)
        tab["testcases"] = translate_testcases(tab["testcases"], language)
    else:
        assert "scripts" in tab
        assert isinstance(tab["scripts"], list)
        tab["scripts"] = translate_testcases(tab["scripts"], language)
    return tab


def translate_tabs(dsl_list: list, language: str) -> list:
    result = []
    for tab in dsl_list:
        assert isinstance(tab, dict)
        result.append(translate_tab(tab, language))

    return result


def translate_dsl(dsl_object: YamlObject, language: str) -> YamlObject:
    if isinstance(dsl_object, list):
        return translate_tabs(dsl_object, language)
    else:
        assert isinstance(dsl_object, dict)
        key_to_set = "units" if "units" in dsl_object else "tabs"
        tab_list = dsl_object.get(key_to_set)
        assert isinstance(tab_list, list)
        dsl_object[key_to_set] = translate_tabs(tab_list, language)
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
