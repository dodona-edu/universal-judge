import os
import sys
from pathlib import Path
from typing import Any, cast

import yaml
from jinja2 import Environment
from yaml.nodes import MappingNode, ScalarNode, SequenceNode

from tested.dsl.translate_parser import (
    DslValidationError,
    ExpressionString,
    ReturnOracle,
    YamlObject,
    _validate_dsl,
    convert_validation_error_to_group,
    load_schema_validator,
    raise_yaml_error,
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


class CustomDumper(yaml.SafeDumper):

    def represent_with_tag(self, tag, value):
        if isinstance(value, dict):
            return self.represent_mapping(tag, value)
        elif isinstance(value, list):
            return self.represent_sequence(tag, value)
        else:
            return self.represent_scalar(tag, value)

    @staticmethod
    def custom_representer(dumper, data):
        if "__tag__" in data:
            if data["__tag__"] != "!programming_language":
                return dumper.represent_with_tag(data["__tag__"], data["value"])
            else:
                data = data["value"]

        return dumper.represent_mapping(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, data
        )


def construct_custom(loader, tag_suffix, node):

    type2method = {
        MappingNode: loader.construct_mapping,
        ScalarNode: loader.construct_scalar,
        SequenceNode: loader.construct_sequence,
    }

    if not type(node) in type2method:
        raise yaml.constructor.ConstructorError(
            None,
            None,
            f"expected a mapping, scalar, or sequence node, but found {node.id}",
            node.start_mark,
        )

    data = type2method[type(node)](node)

    return {"__tag__": tag_suffix, "value": data}


def translate_yaml(
    data: Any, translations: dict, language: str, env: Environment
) -> Any:
    if isinstance(data, dict):
        if "__tag__" in data and data["__tag__"] == "!natural_language":
            return translate_yaml(data["value"][language], translations, language, env)

        current_translations = data.pop("translations", {})
        for key, value in current_translations.items():
            current_translations[key] = value[language]
        merged_translations = {**translations, **current_translations}

        return {
            key: translate_yaml(value, merged_translations, language, env)
            for key, value in data.items()
        }
    elif isinstance(data, list):
        return [translate_yaml(item, translations, language, env) for item in data]
    elif isinstance(data, str):
        return env.from_string(data).render(translations)
    return data


def to_yaml_object(data: Any) -> YamlObject:
    if isinstance(data, dict):
        if "__tag__" in data:
            value = data["value"]
            if data["__tag__"] == "!oracle":
                result = to_yaml_object(value)
                assert isinstance(result, dict)
                return ReturnOracle(result)
            if data["__tag__"] == "!expression":
                return ExpressionString(to_yaml_object(value))
            return value
    elif isinstance(data, list):
        return [to_yaml_object(value) for value in data]

    return data


def wrap_in_braces(value):
    return f"{{{value}}}"


def create_enviroment() -> Environment:
    enviroment = Environment()
    enviroment.filters["braces"] = wrap_in_braces
    return enviroment


def generate_new_yaml(yaml_path: Path, yaml_string: str, language: str):
    file_name = yaml_path.name
    split_name = file_name.split(".")
    path_to_new_yaml = yaml_path.parent / f"{'.'.join(split_name[:-1])}-{language}.yaml"
    with open(path_to_new_yaml, "w") as yaml_file:
        yaml_file.write(yaml_string)


def convert_to_yaml(translated_data: Any) -> str:
    CustomDumper.add_representer(dict, CustomDumper.custom_representer)
    return yaml.dump(
        translated_data, Dumper=CustomDumper, allow_unicode=True, sort_keys=False
    )


def parse_yaml(yaml_stream: str) -> Any:
    """
    Parse a string or stream to YAML.
    """
    loader: type[yaml.Loader] = cast(type[yaml.Loader], yaml.SafeLoader)
    yaml.add_multi_constructor("", construct_custom, loader)

    try:
        return yaml.load(yaml_stream, loader)
    except yaml.MarkedYAMLError as exc:
        raise_yaml_error(yaml_stream, exc)


def run_translation(path: Path, language: str, to_file: bool = True) -> YamlObject:
    try:
        with open(path, "r") as stream:
            yaml_stream = stream.read()
    except FileNotFoundError as e:
        print("The test suite was not found. Check your exercise's config.json file.")
        print(
            "Remember that the test suite is a path relative to the 'evaluation' folder of your exercise."
        )
        raise e
    _, ext = os.path.splitext(path)
    assert ext.lower() in (".yaml", ".yml"), f"expected a yaml file, got {ext}."
    parsed_yaml = parse_yaml(yaml_stream)
    validate_pre_dsl(parsed_yaml)

    enviroment = create_enviroment()
    translated_data = translate_yaml(parsed_yaml, {}, language, enviroment)

    if to_file:
        translated_yaml_string = convert_to_yaml(translated_data)
        generate_new_yaml(path, translated_yaml_string, language)
        return {}
    else:
        yaml_object = to_yaml_object(translated_data)
        _validate_dsl(yaml_object)
        return yaml_object


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected atleast two argument (path to yaml file and language)."

    run_translation(Path(sys.argv[1]), sys.argv[2])
