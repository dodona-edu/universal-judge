import json
import os
import sys
from pathlib import Path
from typing import Any, Type, cast

import jinja2
import yaml
from jinja2 import Environment, TemplateSyntaxError, Undefined, UndefinedError
from jsonschema.protocols import Validator
from jsonschema.validators import validator_for
from yaml.nodes import MappingNode, ScalarNode, SequenceNode

from tested.dodona import ExtendedMessage
from tested.dsl.dsl_errors import (
    build_preprocessor_messages,
    handle_dsl_validation_errors,
    raise_yaml_error,
)


def validate_pre_dsl(yaml_object: Any):
    """
    Validate a DSl object.
    :param yaml_object: The object to validate.
    """
    path_to_schema = Path(__file__).parent / "dsl/schema-strict-nat-translation.json"
    with open(path_to_schema, "r") as schema_file:
        schema_object = json.load(schema_file)

    validator: Type[Validator] = validator_for(schema_object)
    schema_validator = validator(schema_object)
    errors = list(schema_validator.iter_errors(yaml_object))
    handle_dsl_validation_errors(errors)


class CustomTagFormatDumper(yaml.SafeDumper):

    def represent_with_tag(self, tag, value):
        if isinstance(value, dict):
            return self.represent_mapping(tag, value)
        elif isinstance(value, list):
            return self.represent_sequence(tag, value)
        else:
            return self.represent_scalar(tag, value)

    @staticmethod
    def custom_tag_format_representer(dumper, data):
        """
        Will turn the given object back into YAML.

        :param dumper: The dumper to use.
        :param data: The object to represent.
        """
        if "__tag__" in data:
            return dumper.represent_with_tag(data["__tag__"], data["value"])

        return dumper.represent_mapping(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, data
        )


def construct_custom_tag_format(loader, tag_suffix, node):
    """
    This constructor will turn the given YAML into an object that can be used for translation.

    :param loader: The YAML loader.
    :param tag_suffix: The tag that was found.
    :param node: The node to construct.
    """
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


def init_template(
    template: dict,
    translations: dict,
    templates: dict,
    parameters: dict,
    language: str,
    env: Environment,
) -> dict:

    # translate parameters before inserting them into the template
    new_parameters = translate_yaml(
        parameters, translations, templates, parameters, language, env, True
    )

    # translate template and insert parameters
    template = translate_yaml(
        template, translations, templates, new_parameters, language, env, True
    )
    assert isinstance(template, dict)
    return template


def translate_yaml(
    data: Any,
    translations: dict,
    templates: dict,
    parameters: dict,
    language: str,
    env: Environment,
    inside_templates: bool = False,
) -> Any:
    """
    This function will translate the multilingual object.

    :param data: The object to translate.
    :param translations: The merge of all found translations maps.
    :param templates: The merge of all found templates.
    :param parameters: The data passed to a template. This will only not be empty when a template is processed.
    :param language: The language to translate to.
    :param env: The Jinja-environment to use.
    :param inside_templates: Indicator if a template is being processed.
    :return: The translated object.
    """
    if isinstance(data, dict):
        if "__tag__" in data:
            if data["__tag__"] == "!natural_language":
                value = data["value"]
                assert language in value
                return translate_yaml(
                    value[language],
                    translations,
                    templates,
                    parameters,
                    language,
                    env,
                    inside_templates,
                )
            elif data["__tag__"] == "!parameter":
                assert data["value"] in parameters
                return parameters[data["value"]]

        current_templates = data.pop("templates", {})
        templates = {**templates, **current_templates}

        current_translations = data.pop("translations", {})
        for key, value in current_translations.items():
            assert language in value
            current_translations[key] = value[language]
        translations = {**translations, **current_translations}

        if "template" in data or "parameters" in data:
            assert (
                parameters == inside_templates
            ), "A template was defined inside another template. This is not allowed!"
            if "template" in data:
                name = data.pop("template")
                assert name in templates
                template = templates[name]
            else:
                raise ValueError("Found parameter without specifying template!")
            template = init_template(
                template,
                translations,
                templates,
                data.pop("parameters", {}),
                language,
                env,
            )

            if data:
                # Extra specifications in data will overwrite parts of the template.
                data = translate_yaml(
                    data,
                    translations,
                    templates,
                    parameters,
                    language,
                    env,
                    inside_templates,
                )
                for key, value in data.items():
                    template[key] = value
            return template

        if "repeat" in data:
            assert (
                parameters == inside_templates
            ), "A repeat was defined inside another template. This is not allowed!"
            repeat = data.pop("repeat")
            assert "parameters" in repeat
            parameters = repeat["parameters"]
            assert isinstance(parameters, list)

            assert "template" in repeat
            name = repeat.pop("template")
            assert name in templates
            template = templates[name]

            return [
                init_template(
                    template,
                    translations,
                    templates,
                    param_item,
                    language,
                    env,
                )
                for param_item in parameters
            ]

        result = {
            key: translate_yaml(
                value,
                translations,
                templates,
                parameters,
                language,
                env,
                inside_templates,
            )
            for key, value in data.items()
        }
        return result
    elif isinstance(data, list):
        result = []
        for item in data:
            has_repeat = isinstance(item, dict) and "repeat" in item
            translated = translate_yaml(
                item,
                translations,
                templates,
                parameters,
                language,
                env,
                inside_templates,
            )
            if has_repeat and isinstance(translated, list):
                result.extend(translated)
            else:
                result.append(translated)
        return result
    elif isinstance(data, str):
        assert (
            len(set(translations.keys()).intersection(set(parameters.keys()))) == 0
        ), "Found a key in the translations map that is the same as inside a template. Please try to avoid this!"
        try:
            return env.from_string(data).render({**translations, **parameters})
        except TemplateSyntaxError:
            return data

    return data


def wrap_in_braces(value):
    """
    This function will provide the ability to still keep the curly bracket around the
    translated result. Example: {{ key | braces }} => {sleutel} and {{ key }} => sleutel.
    """
    return f"{{{value}}}"


class TrackingUndefined(Undefined):
    missing_keys = list()

    def __str__(self):
        # Store the missing key name
        TrackingUndefined.missing_keys.append(self._undefined_name)
        # Return it in Jinja syntax to keep it in the template
        return f"{{{{ {self._undefined_name} }}}}"

    __repr__ = __str__


def create_enviroment() -> Environment:
    enviroment = Environment(undefined=TrackingUndefined)
    enviroment.filters["braces"] = wrap_in_braces
    return enviroment


def generate_new_yaml(yaml_path: Path, yaml_string: str, language: str):
    file_name = yaml_path.name
    split_name = file_name.split(".")
    path_to_new_yaml = yaml_path.parent / f"{'.'.join(split_name[:-1])}-{language}.yaml"
    with open(path_to_new_yaml, "w", encoding="utf-8") as yaml_file:
        yaml_file.write(yaml_string)


def convert_to_yaml(translated_data: Any) -> str:
    CustomTagFormatDumper.add_representer(
        dict, CustomTagFormatDumper.custom_tag_format_representer
    )
    return yaml.dump(
        translated_data,
        Dumper=CustomTagFormatDumper,
        allow_unicode=True,
        sort_keys=False,
    )


def parse_yaml(yaml_stream: str) -> Any:
    """
    Parse a string or stream to YAML.
    """
    loader: type[yaml.Loader] = cast(type[yaml.Loader], yaml.SafeLoader)
    yaml.add_multi_constructor("", construct_custom_tag_format, loader)

    try:
        return yaml.load(yaml_stream, loader)
    except yaml.MarkedYAMLError as exc:
        raise_yaml_error(yaml_stream, exc)


def apply_translations(
    yaml_stream: str, language: str
) -> tuple[str, list[ExtendedMessage]]:
    parsed_yaml = parse_yaml(yaml_stream)
    validate_pre_dsl(parsed_yaml)

    enviroment = create_enviroment()
    translated_data = translate_yaml(parsed_yaml, {}, {}, {}, language, enviroment)

    missing_keys = TrackingUndefined.missing_keys
    messages = build_preprocessor_messages(missing_keys)
    translated_yaml_string = convert_to_yaml(translated_data)
    return translated_yaml_string, messages


def translate_file(path: Path, language: str) -> tuple[str, list[ExtendedMessage]]:
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
    translated_yaml_string, messages = apply_translations(yaml_stream, language)
    generate_new_yaml(path, translated_yaml_string, language)
    return translated_yaml_string, messages


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected atleast two argument (path to yaml file and language)."

    translate_file(Path(sys.argv[1]), sys.argv[2])
