import sys
from pathlib import Path

import yaml
from jinja2 import Environment

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


def natural_language_map_translation(value: YamlObject, language: str):
    if isinstance(value, NaturalLanguageMap):
        assert language in value
        value = value[language]
    return value


def translate_input_files(
    dsl_object: dict, language: str, flattened_stack: dict, env: Environment
) -> dict:
    if (files := dsl_object.get("files")) is not None:
        # Translation map can happen at the top level.
        files = natural_language_map_translation(files, language)
        assert isinstance(files, list)
        for i in range(len(files)):
            file = files[i]

            # Do the formatting.
            if isinstance(file, dict):
                name = file["name"]
                assert isinstance(name, str)
                file["name"] = format_string(name, flattened_stack, env)
                url = file["url"]
                assert isinstance(url, str)
                file["url"] = format_string(url, flattened_stack, env)
            files[i] = file

        dsl_object["files"] = files
    return dsl_object


def parse_value(
    value: YamlObject, flattened_stack: dict, env: Environment
) -> YamlObject:

    # Will format the strings in different values.
    if isinstance(value, str):
        return format_string(value, flattened_stack, env)
    elif isinstance(value, dict):
        return {k: parse_value(v, flattened_stack, env) for k, v in value.items()}
    elif isinstance(value, list):
        return [parse_value(v, flattened_stack, env) for v in value]

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


def format_string(string: str, translations: dict, env: Environment) -> str:
    template = env.from_string(string)
    result = template.render(translations)
    # print(f"jinja result: {result}")

    # return string.format(**translations)
    return result


def translate_io(
    io_object: YamlObject, key: str, language: str, flat_stack: dict, env: Environment
) -> YamlObject:
    # Translate NaturalLanguageMap
    io_object = natural_language_map_translation(io_object, language)

    if isinstance(io_object, dict):
        data = natural_language_map_translation(io_object[key], language)
        io_object[key] = parse_value(data, flat_stack, env)

    # Perform translation based of translation stack.
    elif isinstance(io_object, str):
        return format_string(io_object, flat_stack, env)

    return io_object


def translate_testcase(
    testcase: YamlDict, language: str, translation_stack: list, env: Environment
) -> YamlDict:
    _validate_testcase_combinations(testcase)
    flat_stack = flatten_stack(translation_stack, language)

    key_to_set = "statement" if "statement" in testcase else "expression"
    if (expr_stmt := testcase.get(key_to_set)) is not None:
        # Program language translation found
        if isinstance(expr_stmt, ProgrammingLanguageMap):
            expr_stmt = {
                k: natural_language_map_translation(v, language)
                for k, v in expr_stmt.items()
            }
        elif isinstance(
            expr_stmt, NaturalLanguageMap
        ):  # Natural language translation found
            assert language in expr_stmt
            expr_stmt = expr_stmt[language]

        testcase[key_to_set] = parse_value(expr_stmt, flat_stack, env)
    else:
        if (stdin_stmt := testcase.get("stdin")) is not None:
            # Translate NaturalLanguageMap
            stdin_stmt = natural_language_map_translation(stdin_stmt, language)

            # Perform translation based of translation stack.
            assert isinstance(stdin_stmt, str)
            testcase["stdin"] = format_string(stdin_stmt, flat_stack, env)

        # Translate NaturalLanguageMap
        arguments = testcase.get("arguments", [])
        arguments = natural_language_map_translation(arguments, language)

        # Perform translation based of translation stack.
        assert isinstance(arguments, list)
        testcase["arguments"] = parse_value(arguments, flat_stack, env)

    if (stdout := testcase.get("stdout")) is not None:
        testcase["stdout"] = translate_io(stdout, "data", language, flat_stack, env)

    if (file := testcase.get("file")) is not None:
        # Translate NaturalLanguageMap
        file = natural_language_map_translation(file, language)

        assert isinstance(file, dict)
        file["content"] = format_string(str(file["content"]), flat_stack, env)
        file["location"] = format_string(str(file["location"]), flat_stack, env)

        testcase["file"] = file

    if (stderr := testcase.get("stderr")) is not None:
        testcase["stderr"] = translate_io(stderr, "data", language, flat_stack, env)

    if (exception := testcase.get("exception")) is not None:
        testcase["exception"] = translate_io(
            exception, "message", language, flat_stack, env
        )

    if (result := testcase.get("return")) is not None:
        if isinstance(result, ReturnOracle):
            arguments = result.get("arguments", [])
            arguments = natural_language_map_translation(arguments, language)

            # Perform translation based of translation stack.
            result["arguments"] = parse_value(arguments, flat_stack, env)

            value = result.get("value")
            value = natural_language_map_translation(value, language)

            result["value"] = parse_value(value, flat_stack, env)
            testcase["return"] = result

        elif isinstance(result, NaturalLanguageMap):
            assert language in result
            testcase["return"] = parse_value(result[language], flat_stack, env)
        elif result is not None:
            testcase["return"] = parse_value(result, flat_stack, env)

    if (description := testcase.get("description")) is not None:
        description = natural_language_map_translation(description, language)

        if isinstance(description, str):
            testcase["description"] = format_string(description, flat_stack, env)
        else:
            assert isinstance(description, dict)
            dd = description["description"]
            dd = natural_language_map_translation(dd, language)

            assert isinstance(dd, str)
            description["description"] = format_string(dd, flat_stack, env)

    testcase = translate_input_files(testcase, language, flat_stack, env)

    return testcase


def translate_testcases(
    testcases: list, language: str, translation_stack: list, env: Environment
) -> list:
    result = []
    for testcase in testcases:
        assert isinstance(testcase, dict)
        result.append(translate_testcase(testcase, language, translation_stack, env))

    return result


def translate_contexts(
    contexts: list, language: str, translation_stack: list, env: Environment
) -> list:
    result = []
    for context in contexts:
        assert isinstance(context, dict)

        # Add translation to stack
        if "translations" in context:
            translation_stack.append(context["translations"])

        key_to_set = "script" if "script" in context else "testcases"
        raw_testcases = context.get(key_to_set)
        assert isinstance(raw_testcases, list)
        context[key_to_set] = translate_testcases(
            raw_testcases, language, translation_stack, env
        )

        flat_stack = flatten_stack(translation_stack, language)
        context = translate_input_files(context, language, flat_stack, env)
        result.append(context)

        # Pop translation from stack
        if "translations" in context:
            translation_stack.pop()
            context.pop("translations")

    return result


def translate_tab(
    tab: YamlDict, language: str, translation_stack: list, env: Environment
) -> YamlDict:
    key_to_set = "unit" if "unit" in tab else "tab"
    name = tab.get(key_to_set)
    name = natural_language_map_translation(name, language)

    assert isinstance(name, str)
    flat_stack = flatten_stack(translation_stack, language)
    tab[key_to_set] = format_string(name, flat_stack, env)

    tab = translate_input_files(tab, language, flat_stack, env)

    # The tab can have testcases or contexts.
    if "contexts" in tab:
        assert isinstance(tab["contexts"], list)
        tab["contexts"] = translate_contexts(
            tab["contexts"], language, translation_stack, env
        )
    elif "cases" in tab:
        assert "unit" in tab
        # We have testcases N.S. / contexts O.S.
        assert isinstance(tab["cases"], list)
        tab["cases"] = translate_contexts(
            tab["cases"], language, translation_stack, env
        )
    elif "testcases" in tab:
        # We have scripts N.S. / testcases O.S.
        assert "tab" in tab
        assert isinstance(tab["testcases"], list)
        tab["testcases"] = translate_testcases(
            tab["testcases"], language, translation_stack, env
        )
    else:
        assert "scripts" in tab
        assert isinstance(tab["scripts"], list)
        tab["scripts"] = translate_testcases(
            tab["scripts"], language, translation_stack, env
        )
    return tab


def translate_tabs(
    dsl_list: list, language: str, env: Environment, translation_stack=None
) -> list:
    if translation_stack is None:
        translation_stack = []

    result = []
    for tab in dsl_list:
        assert isinstance(tab, dict)

        if "translations" in tab:
            translation_stack.append(tab["translations"])

        result.append(translate_tab(tab, language, translation_stack, env))
        if "translations" in tab:
            translation_stack.pop()
            tab.pop("translations")

    return result


def wrap_in_braces(value):
    return f"{{{value}}}"


def create_enviroment() -> Environment:
    enviroment = Environment()
    enviroment.filters["braces"] = wrap_in_braces
    return enviroment


def translate_dsl(dsl_object: YamlObject, language: str) -> YamlObject:

    env = create_enviroment()

    if isinstance(dsl_object, list):
        return translate_tabs(dsl_object, language, env)
    else:
        assert isinstance(dsl_object, dict)
        key_to_set = "units" if "units" in dsl_object else "tabs"
        tab_list = dsl_object.get(key_to_set)
        assert isinstance(tab_list, list)
        translation_stack = []
        if "translations" in dsl_object:
            translation_stack.append(dsl_object["translations"])
            dsl_object.pop("translations")

        flat_stack = flatten_stack(translation_stack, language)
        dsl_object = translate_input_files(dsl_object, language, flat_stack, env)
        dsl_object[key_to_set] = translate_tabs(
            tab_list, language, env, translation_stack
        )
        return dsl_object


def parse_yaml(yaml_path: Path) -> YamlObject:
    with open(yaml_path, "r") as stream:
        result = _parse_yaml(stream.read())

    return result


def generate_new_yaml(yaml_path: Path, yaml_string: str, language: str):
    file_name = yaml_path.name
    split_name = file_name.split(".")
    path_to_new_yaml = yaml_path.parent / f"{'.'.join(split_name[:-1])}-{language}.yaml"
    with open(path_to_new_yaml, "w") as yaml_file:
        yaml_file.write(yaml_string)


def convert_to_yaml(yaml_object: YamlObject) -> str:
    def oracle_representer(dumper, data):
        return dumper.represent_mapping("!oracle", data)

    def expression_representer(dumper, data):
        return dumper.represent_scalar("!expression", data)

    # def represent_str(dumper, data):
    #     return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='"')

    # Register the representer for the ReturnOracle object
    yaml.add_representer(ReturnOracle, oracle_representer)
    yaml.add_representer(ExpressionString, expression_representer)
    # yaml.add_representer(str, represent_str)
    return yaml.dump(yaml_object, sort_keys=False)


def run(path: Path, language: str):
    new_yaml = parse_yaml(path)
    validate_pre_dsl(new_yaml)
    translated_dsl = translate_dsl(new_yaml, language)
    yaml_string = convert_to_yaml(translated_dsl)
    _validate_dsl(_parse_yaml(yaml_string))

    generate_new_yaml(path, yaml_string, language)


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected atleast two argument (path to yaml file and language)."

    run(Path(sys.argv[1]), sys.argv[2])
