import copy
import sys
import textwrap
from collections import deque
from pathlib import Path
from typing import Any, Hashable

import yaml
from jinja2 import Environment

from tested.datatypes import AllTypes
from tested.dsl.translate_parser import (
    ExpressionString,
    ProgrammingLanguageMap,
    ReturnOracle,
    YamlObject,
    _custom_type_constructors,
    _expression_string,
    _parse_yaml,
    _return_oracle,
    _validate_dsl,
)
from tested.utils import get_args


class State:
    def __init__(
        self, children: int, translations_stack: list, nat_language_indicator: list
    ):
        self.nat_language_of_lists_indicator = nat_language_indicator
        self.translations_stack = translations_stack
        self.children = children
        self.total_children = 0
        for i in range(self.children):
            if i < len(self.nat_language_of_lists_indicator):
                self.total_children += self.nat_language_of_lists_indicator[i]
            else:
                self.total_children += 1

    def is_finished(self) -> bool:
        return self.total_children == 0


class StateLoader(yaml.SafeLoader):
    def __init__(self, stream):
        super().__init__(stream)
        self.level_state = 0
        self.lang = ""
        self.state_queue: deque[State] = deque()
        start_state = State(1, [], [])
        self.state_queue.append(start_state)
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
        translation_stack = copy.deepcopy(self.state_queue[0].translations_stack)

        # These checks are here to make sure that the order between tabs, a tab
        # and testcases is preserved.
        if "tabs" in result or "units" in result:
            assert (
                self.level_state == 0
            ), "Tabs can't be redefined or can't be defined when a tab is already defined."
            self.level_state = 1
            self.state_queue.popleft()

        elif "tab" in result or "unit" in result:
            assert (
                self.level_state < 2
            ), "Can't define a tab when a context or testcases are already defined."
            self.level_state = 1

        elif "testcases" in result or "scripts" in result:

            if self.level_state == 1:
                self.level_state = 2
            assert (
                self.level_state == 2
            ), "Can't define a context when when a tab isn't defined yet."

        children = self.count_children(result)

        if "translations" in result:
            translation_stack.append(
                translate_translations_map(result.pop("translations"), self.lang)
            )

        trans_map = flatten_stack(translation_stack)
        result = parse_dict(result, trans_map, self.env)

        if children > 0:
            new_state = State(children, translation_stack, self.nat_language_indicator)
            self.state_queue.append(new_state)
            self.nat_language_indicator = []

        return result

    def construct_sequence(self, node: yaml.SequenceNode, deep=False) -> list[Any]:
        result = super().construct_sequence(node, deep)

        translation_stack = self.state_queue[0].translations_stack
        trans_map = flatten_stack(translation_stack)
        result = parse_list(result, trans_map, self.env)

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


def natural_language_map(loader: StateLoader, node: yaml.MappingNode) -> Any:
    result = loader.construct_mapping(node)

    children = loader.count_children(result)
    loader.add_nat_language_indication(children)

    return result[loader.lang]


def _programming_language_map(
    loader: StateLoader, node: yaml.MappingNode
) -> ProgrammingLanguageMap:
    result = loader.construct_mapping(node)
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
        loader.add_constructor("!natural_language", natural_language_map)
        loader.add_constructor("!programming_language", _programming_language_map)
        # This line is need because otherwise there won't
        # be a full translations map on time.
        loader.add_constructor(
            yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, dict_trans
        )

        return loader.get_data()
    except yaml.MarkedYAMLError as exc:
        lines = yaml_stream.splitlines()

        if exc.problem_mark is None:
            # There is no additional information, so what can we do?
            raise exc

        sys.stderr.write(
            textwrap.dedent(
                f"""
        YAML error while parsing test suite. This means there is a YAML syntax error.

        The YAML parser indicates the problem lies at line {exc.problem_mark.line + 1}, column {exc.problem_mark.column + 1}:

            {lines[exc.problem_mark.line]}
            {" " * exc.problem_mark.column + "^"}

        The error message was:
            {exc.problem} {exc.context}

        The detailed exception is provided below.
        You might also find help by validating your YAML file with a YAML validator.\n
        """
            )
        )
        raise exc


def parse_yaml(yaml_path: Path, language: str) -> YamlObject:
    with open(yaml_path, "r") as stream:
        result = translate_yaml(stream.read(), language)

    return result


def run(path: Path, language: str):
    new_yaml = parse_yaml(path, language)
    yaml_string = convert_to_yaml(new_yaml)
    _validate_dsl(_parse_yaml(yaml_string))
    generate_new_yaml(path, yaml_string, language)


if __name__ == "__main__":
    n = len(sys.argv)
    assert n > 1, "Expected atleast two argument (path to yaml file and language)."

    run(Path(sys.argv[1]), sys.argv[2])

