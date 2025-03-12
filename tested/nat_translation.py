import os
import sys
from collections import deque
from pathlib import Path
from typing import Any, Hashable, cast

import yaml
from jinja2 import Environment

from tested.datatypes import AllTypes
from tested.dsl.translate_parser import (
    DslValidationError,
    ExpressionString,
    NaturalLanguageMap,
    ProgrammingLanguageMap,
    ReturnOracle,
    YamlObject,
    _custom_type_constructors,
    _expression_string,
    _parse_yaml,
    _parse_yaml_value,
    _return_oracle,
    _validate_dsl,
    convert_validation_error_to_group,
    load_schema_validator,
    raise_yaml_error,
)
from tested.utils import get_args


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


class State:
    def __init__(
        self, children: int, translations_map: dict, nat_language_indicator: list
    ):
        self.nat_language_of_lists_indicator = nat_language_indicator
        self.translations_map = translations_map
        self.total_children = 0
        for i in range(children):
            if i < len(self.nat_language_of_lists_indicator):
                self.total_children += self.nat_language_of_lists_indicator[i]
            else:
                self.total_children += 1

    def is_finished(self) -> bool:
        return self.total_children == 0


class StateLoader(yaml.SafeLoader):
    def __init__(self, stream):
        super().__init__(stream)
        self.lang = ""
        self.state_queue: deque[State] = deque()
        self.nat_language_indicator = []
        self.env = create_enviroment()

    @staticmethod
    def count_children(dictionary: dict):
        # The children of a dictionary will always be stored into a list that will
        # be empty at the moment it is being parsed.
        return sum(1 for v in dictionary.values() if isinstance(v, list) and not v)

    def set_language(self, lang: str):
        self.lang = lang

    def add_nat_language_indication(self, children: int):
        self.nat_language_indicator.append(children)
        if children > 0:
            self.state_queue.pop()

    def construct_mapping(
        self, node: yaml.MappingNode, deep=False
    ) -> dict[Hashable, Any]:
        # This method will run for each map in a YamlObject.
        result = super().construct_mapping(node, deep)
        new_translations_map = {}
        if len(self.state_queue) > 0:
            new_translations_map = self.state_queue[0].translations_map

        if "translations" in result:
            new_translations_map = flatten_stack(
                [
                    new_translations_map,
                    translate_translations_map(result.pop("translations"), self.lang),
                ]
            )

        children = self.count_children(result)
        result = parse_dict(result, new_translations_map, self.env)

        if children > 0:
            new_state = State(
                children, new_translations_map, self.nat_language_indicator
            )
            self.state_queue.append(new_state)
            self.nat_language_indicator = []

        return result

    def construct_sequence(self, node: yaml.SequenceNode, deep=False) -> list[Any]:
        result = super().construct_sequence(node, deep)

        result = parse_list(result, self.state_queue[0].translations_map, self.env)

        self.state_queue[0].total_children -= 1
        if self.state_queue[0].is_finished():
            self.state_queue.popleft()

        return result


def parse_value(
    value: YamlObject, flattened_stack: dict, env: Environment
) -> YamlObject:

    if isinstance(value, str):
        return type(value)(format_string(value, flattened_stack, env))

    return value


def parse_list(value: list[Any], flattened_stack: dict, env: Environment) -> list[Any]:
    if len(value) > 0:
        return [parse_value(v, flattened_stack, env) for v in value]
    return value


def parse_dict(
    value: dict[Hashable, Any], flattened_stack: dict, env: Environment
) -> dict[Hashable, Any]:
    return {k: parse_value(v, flattened_stack, env) for k, v in value.items()}


def flatten_stack(translation_stack: list) -> dict:
    # Will transform a list of translation maps into a dict that
    # has all the keys defined over all the different translation map and will have
    # the value of the newest definition. In this definition we also chose
    # the translation of the provided language.
    flattened = {}
    for d in translation_stack:
        flattened.update({k: v for k, v in d.items()})
    return flattened


def format_string(string: str, translations: dict, env: Environment) -> str:
    template = env.from_string(string)
    result = template.render(translations)

    return result


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


def convert_to_yaml(yaml_object: YamlObject) -> str:
    def oracle_representer(dumper, data):
        return dumper.represent_mapping("!oracle", data)

    def expression_representer(dumper, data):
        return dumper.represent_scalar("!expression", data)

    def programming_language_map_representer(dumper, data):
        return dumper.represent_mapping("tag:yaml.org,2002:map", dict(data))

    yaml.add_representer(ReturnOracle, oracle_representer)
    yaml.add_representer(ExpressionString, expression_representer)
    yaml.add_representer(ProgrammingLanguageMap, programming_language_map_representer)
    return yaml.dump(yaml_object, sort_keys=False)


def translate_map(value: YamlObject, language: str):
    assert isinstance(
        value, dict
    ), "The translation map does not consist of dictionaries."
    assert language in value
    value = value[language]
    return value


def translate_translations_map(trans_map: dict, language: str) -> dict:
    return {k: translate_map(v, language) for k, v in trans_map.items()}


def _natural_language_map_translation(
    loader: StateLoader, node: yaml.MappingNode
) -> Any:
    result = loader.construct_mapping(node)

    children = loader.count_children(result)
    loader.add_nat_language_indication(children)
    return result[loader.lang]


def _natural_language_map(loader: yaml.Loader, node: yaml.Node) -> NaturalLanguageMap:
    result = _parse_yaml_value(loader, node)
    assert isinstance(
        result, dict
    ), f"A natural language map must be an object, got {result} which is a {type(result)}."
    return NaturalLanguageMap(result)


def _programming_language_map(
    loader: yaml.Loader, node: yaml.Node
) -> ProgrammingLanguageMap:
    result = _parse_yaml_value(loader, node)
    assert isinstance(
        result, dict
    ), f"A programming language map must be an object, got {result} which is a {type(result)}."
    return ProgrammingLanguageMap(result)


def dict_trans(loader: StateLoader, node: yaml.MappingNode) -> dict[Hashable, Any]:
    return loader.construct_mapping(node)


def translate_yaml(yaml_stream: str, language: str) -> YamlObject:
    """
    Parse a string or stream to YAML.
    """
    try:
        loader = StateLoader(yaml_stream)
        loader.set_language(language)
        for types in get_args(AllTypes):
            for actual_type in types:
                loader.add_constructor("!" + actual_type, _custom_type_constructors)
        loader.add_constructor("!expression", _expression_string)
        loader.add_constructor("!oracle", _return_oracle)
        loader.add_constructor("!natural_language", _natural_language_map_translation)
        loader.add_constructor("!programming_language", _programming_language_map)
        # This line is need because otherwise there won't
        # be a full translations map on time.
        loader.add_constructor(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, dict_trans
        )

        return loader.get_data()
    except yaml.MarkedYAMLError as exc:
        raise_yaml_error(yaml_stream, exc)


def parse_yaml(yaml_stream: str) -> YamlObject:
    """
    Parse a string or stream to YAML.
    """
    loader: type[yaml.Loader] = cast(type[yaml.Loader], yaml.CSafeLoader)
    for types in get_args(AllTypes):
        for actual_type in types:
            yaml.add_constructor("!" + actual_type, _custom_type_constructors, loader)
    yaml.add_constructor("!expression", _expression_string, loader)
    yaml.add_constructor("!oracle", _return_oracle, loader)
    yaml.add_constructor("!natural_language", _natural_language_map, loader)
    yaml.add_constructor("!programming_language", _programming_language_map, loader)

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
    yaml_object = parse_yaml(yaml_stream)
    validate_pre_dsl(yaml_object)

    translated_yaml_ob = translate_yaml(yaml_stream, language)
    translated_yaml_string = convert_to_yaml(translated_yaml_ob)
    _validate_dsl(_parse_yaml(translated_yaml_string))
    if to_file:
        generate_new_yaml(path, translated_yaml_string, language)

    return translated_yaml_ob


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected atleast two argument (path to yaml file and language)."

    run_translation(Path(sys.argv[1]), sys.argv[2])
