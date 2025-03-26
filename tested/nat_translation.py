import json
import os
import re
import sys

# import pystache
from pathlib import Path
from typing import Any, Type, cast

import yaml
from jinja2 import Environment, TemplateSyntaxError
from jsonschema.protocols import Validator
from jsonschema.validators import validator_for
from yaml.nodes import MappingNode, ScalarNode, SequenceNode

from tested.dsl.dsl_errors import handle_dsl_validation_errors, raise_yaml_error
from tested.dsl.translate_parser import (
    ExpressionString,
    ReturnOracle,
    YamlObject,
    _validate_dsl,
)


def validate_pre_dsl(yaml_object: Any):
    """
    Validate a DSl object.
    :param yaml_object: The object to validate.
    :return: True if valid, False otherwise.
    """
    path_to_schema = Path(__file__).parent / "dsl/schema-strict-nat-translation.json"
    with open(path_to_schema, "r") as schema_file:
        schema_object = json.load(schema_file)

    validator: Type[Validator] = validator_for(schema_object)
    schema_validator = validator(schema_object)
    errors = list(schema_validator.iter_errors(yaml_object))
    handle_dsl_validation_errors(errors)


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
    elif isinstance(data, str) and translations:
        # return pystache.render(data, translations)
        return env.from_string(data).render(translations)
        # try:
        #     data = "{% raw %}" + data + "{% endraw %}"
        #     return env.from_string(data).render(translations)
        # except TemplateSyntaxError as e:
        #     print(f"error: {e}")
        #     print(f"data {data}")
        #     return re.sub(r"{{\s*(.*?)\s*}}",
        #                   lambda m: translations.get(m.group(1).strip(), m.group(0)),
        #                   data)
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
            return to_yaml_object(value)
        return {k: to_yaml_object(v) for k, v in data.items()}
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
    with open(path_to_new_yaml, "w", encoding="utf-8") as yaml_file:
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
