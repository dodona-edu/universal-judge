import json
import os
import sys
from pathlib import Path
from typing import Any, Type, cast

import yaml
from jinja2 import Environment, TemplateSyntaxError, Undefined
from jsonschema.protocols import Validator
from jsonschema.validators import validator_for
from yaml.nodes import MappingNode, ScalarNode, SequenceNode

from tested.dsl.dsl_errors import handle_dsl_validation_errors, raise_yaml_error


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


def construct_custom(loader, tag_suffix, node):
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


def translate_yaml(
    data: Any,
    translations: dict,
    templates: dict,
    template_data: dict,
    language: str,
    env: Environment,
) -> Any:
    """
    This function will translate the multilingual object.

    :param data: The object to translate.
    :param translations: The merge of all found translations maps.
    :param templates: The merge of all found templates.
    :param template_data: The data passed to a template. This will only not be empty when a template is processed.
    :param language: The language to translate to.
    :param env: The Jinja-environment to use.
    :return: The translated object.
    """
    if isinstance(data, dict):
        if "__tag__" in data and data["__tag__"] == "!natural_language":
            value = data["value"]
            assert language in value
            return translate_yaml(
                value[language], translations, templates, template_data, language, env
            )

        current_templates = data.pop("templates", {})
        templates = {**templates, **current_templates}

        current_translations = data.pop("translations", {})
        for key, value in current_translations.items():
            assert language in value
            current_translations[key] = value[language]
        translations = {**translations, **current_translations}

        template = None
        if "placeholder" in data:
            placeholder = data.pop("placeholder")
            name = placeholder["name"]
            new_temp_data = translate_yaml(
                placeholder["data"],
                translations,
                templates,
                template_data,
                language,
                env,
            )

            assert name in templates
            template = translate_yaml(
                templates[name], translations, templates, new_temp_data, language, env
            )

            if not data:
                return template
            assert isinstance(template, dict)

        result = {
            key: translate_yaml(
                value, translations, templates, template_data, language, env
            )
            for key, value in data.items()
        }
        if template is not None:
            result.update(template)

        return result
    elif isinstance(data, list):
        return [
            translate_yaml(item, translations, templates, template_data, language, env)
            for item in data
        ]
    elif isinstance(data, str):
        print(data)
        try:
            return env.from_string(data).render(
                translations=translations, template=template_data
            )
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


def run_translation(
    path: Path, language: str, to_file: bool = True
) -> tuple[str, list]:
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
    #validate_pre_dsl(parsed_yaml)

    enviroment = create_enviroment()
    translated_data = translate_yaml(parsed_yaml, {}, {}, {}, language, enviroment)

    missing_keys = TrackingUndefined.missing_keys
    translated_yaml_string = convert_to_yaml(translated_data)
    if to_file:
        generate_new_yaml(path, translated_yaml_string, language)
        return "", missing_keys
    else:
        return translated_yaml_string, missing_keys


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected atleast two argument (path to yaml file and language)."

    run_translation(Path(sys.argv[1]), sys.argv[2])
