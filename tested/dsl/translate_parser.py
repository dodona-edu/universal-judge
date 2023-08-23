import json
from decimal import Decimal
from logging import getLogger
from pathlib import Path
from typing import (
    Any,
    Callable,
    Dict,
    List,
    Literal,
    Optional,
    TextIO,
    TypeVar,
    Union,
    cast,
)

import yaml
from attrs import define, evolve
from jsonschema.exceptions import ValidationError
from jsonschema.validators import Draft201909Validator

from tested.datatypes import (
    AdvancedNumericTypes,
    AllTypes,
    BasicBooleanTypes,
    BasicNothingTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
    BooleanTypes,
    NothingTypes,
    NumericTypes,
    ObjectTypes,
    SequenceTypes,
    StringTypes,
    resolve_to_basic,
)
from tested.dsl.ast_translator import parse_string
from tested.parsing import get_converter, suite_to_json
from tested.serialisation import (
    BooleanType,
    NothingType,
    NumberType,
    ObjectKeyValuePair,
    ObjectType,
    SequenceType,
    StringType,
    Value,
)
from tested.testsuite import (
    Context,
    CustomCheckOracle,
    EmptyChannel,
    EvaluationFunction,
    ExceptionOutputChannel,
    ExitCodeOutputChannel,
    ExpectedException,
    FileUrl,
    GenericTextOracle,
    IgnoredChannel,
    LanguageLiterals,
    MainInput,
    Output,
    Suite,
    SupportedLanguage,
    Tab,
    Testcase,
    TextData,
    TextOutputChannel,
    ValueOutputChannel,
)
from tested.utils import get_args, recursive_dict_merge

logger = getLogger(__name__)

OptionDict = Dict[str, Union[int, bool]]
YamlDict = Dict[str, "YamlObject"]
YamlObject = Union[YamlDict, list, bool, float, int, str, None]


@define
class TestedType:
    value: Any
    type: str | AllTypes


@define
class YamlValue:
    value: Any


def _parse_yaml_value(loader: yaml.Loader, node: yaml.Node) -> Any:
    if isinstance(node, yaml.MappingNode):
        result = loader.construct_mapping(node)
    elif isinstance(node, yaml.SequenceNode):
        result = loader.construct_sequence(node)
    else:
        assert isinstance(node, yaml.ScalarNode)
        result = loader.construct_scalar(node)
    return result


def _custom_type_constructors(loader: yaml.Loader, node: yaml.Node) -> TestedType:
    tested_tag = node.tag[1:]
    base_result = _parse_yaml_value(loader, node)
    return TestedType(type=tested_tag, value=base_result)


def _yaml_value_constructor(loader: yaml.Loader, node: yaml.Node) -> YamlValue:
    return YamlValue(value=_parse_yaml_value(loader, node))


def _parse_yaml(yaml_stream: Union[str, TextIO]) -> YamlObject:
    """
    Parse a string or stream to YAML.
    """
    loader: type[yaml.Loader] = cast(type[yaml.Loader], yaml.CSafeLoader)
    for types in get_args(AllTypes):
        for actual_type in types:
            yaml.add_constructor("!" + actual_type, _custom_type_constructors, loader)
    yaml.add_constructor("!v", _yaml_value_constructor, loader)
    return yaml.load(yaml_stream, loader)


def _load_schema_validator():
    """
    Load the JSON Schema validator used to check DSL test suites.
    """
    path_to_schema = Path(__file__).parent / "schema.json"
    with open(path_to_schema, "r") as schema_file:
        schema_object = json.load(schema_file)
    Draft201909Validator.check_schema(schema_object)
    return Draft201909Validator(schema_object)


_SCHEMA_VALIDATOR = _load_schema_validator()


class DslValidationError(ValueError):
    pass


@define(frozen=True)
class DslContext:
    """
    Carries context in each level.
    """

    config: dict
    files: List[FileUrl]
    language: Union[SupportedLanguage, Literal["tested"]] = "tested"

    def deepen_context(
        self,
        new_level: Optional[YamlDict],
        include_config=True,
        include_files=True,
        merge_with: Optional[str] = None,
    ) -> "DslContext":
        """
        Merge certain fields of the new object with the current context, resulting
        in a new context for the new level.

        :param new_level: The new object from the DSL to get information from.
        :param include_config: If config should be considered.
        :param include_files: If files should be considered.
        :param merge_with: If merging configs, the key of the subsection to update.

        :return: A new context.
        """
        if new_level is None:
            return self

        new_config = self.config
        if include_config and "config" in new_level:
            assert isinstance(new_level["config"], dict)
            if merge_with:
                original_config = self.config.get(merge_with, {})
            else:
                original_config = self.config

            new_config = recursive_dict_merge(original_config, new_level["config"])

        new_files = self.files
        if include_files and "files" in new_level:
            assert isinstance(new_level["files"], list)
            additional_files = {_convert_file(f) for f in new_level["files"]}
            new_files = list(set(self.files) | additional_files)

        return evolve(self, config=new_config, files=new_files)


def convert_validation_error_to_group(
    error: ValidationError,
) -> ExceptionGroup | Exception:
    if not error.context and not error.cause:
        if len(error.message) > 150:
            message = error.message.replace(str(error.instance), "<DSL>")
            note = "With <DSL> being: " + str(error.instance)
        else:
            message = error.message
            note = None
        converted = DslValidationError(
            f"Validation error at {error.json_path}: " + message
        )
        if note:
            converted.add_note(note)
        return converted
    elif error.cause:
        return error.cause
    elif error.context:
        causes = [convert_validation_error_to_group(x) for x in error.context]
        message = f"Validation error at {error.json_path}, caused by a sub-exception."
        return ExceptionGroup(message, causes)
    else:
        return error


def _validate_dsl(dsl_object: YamlObject):
    """
    Validate a DSl object.

    :param dsl_object: The object to validate.
    :return: True if valid, False otherwise.
    """
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


def _tested_type_to_value(tested_type: TestedType) -> Value:
    type_enum = get_converter().structure(tested_type.type, AllTypes)
    if isinstance(type_enum, NumericTypes):
        # Some special cases for advanced numeric types.
        if type_enum == AdvancedNumericTypes.FIXED_PRECISION:
            value = Decimal(tested_type.value)
        else:
            basic_type = resolve_to_basic(type_enum)
            if basic_type == BasicNumericTypes.INTEGER:
                value = int(tested_type.value)
            elif basic_type == BasicNumericTypes.REAL:
                value = float(tested_type.value)
            else:
                raise ValueError(f"Unknown basic numeric type {type_enum}")
        return NumberType(type=type_enum, data=value)
    elif isinstance(type_enum, StringTypes):
        return StringType(type=type_enum, data=tested_type.value)
    elif isinstance(type_enum, BooleanTypes):
        return BooleanType(type=type_enum, data=bool(tested_type.value))
    elif isinstance(type_enum, NothingTypes):
        return NothingType(type=type_enum, data=None)
    elif isinstance(type_enum, SequenceTypes):
        return SequenceType(
            type=type_enum,
            data=[_convert_value(part_value) for part_value in tested_type.value],
        )
    elif isinstance(type_enum, ObjectTypes):
        data = []
        for key, val in tested_type.value.items():
            data.append(
                ObjectKeyValuePair(
                    key=_convert_value(key),
                    value=_convert_value(val),
                )
            )
        return ObjectType(type=type_enum, data=data)
    raise ValueError(f"Unknown type {tested_type.type} with value {tested_type.value}")


def _convert_value(value: YamlObject) -> Value:
    if isinstance(value, TestedType):
        tested_type = value
    else:
        # Convert the value into a "TESTed" type.
        if value is None:
            tested_type = TestedType(value=None, type=BasicNothingTypes.NOTHING)
        elif isinstance(value, str):
            tested_type = TestedType(value=value, type=BasicStringTypes.TEXT)
        elif isinstance(value, bool):
            tested_type = TestedType(type=BasicBooleanTypes.BOOLEAN, value=value)
        elif isinstance(value, int):
            tested_type = TestedType(type=BasicNumericTypes.INTEGER, value=value)
        elif isinstance(value, float):
            tested_type = TestedType(type=BasicNumericTypes.REAL, value=value)
        elif isinstance(value, list):
            tested_type = TestedType(type=BasicSequenceTypes.SEQUENCE, value=value)
        elif isinstance(value, set):
            tested_type = TestedType(type=BasicSequenceTypes.SET, value=value)
        elif isinstance(value, dict):
            tested_type = TestedType(type=BasicObjectTypes.MAP, value=value)
        else:
            raise ValueError(f"Unknown type for value {value}.")

    return _tested_type_to_value(tested_type)


def _convert_file(link_file: YamlDict) -> FileUrl:
    assert isinstance(link_file["name"], str)
    assert isinstance(link_file["url"], str)
    return FileUrl(name=link_file["name"], url=link_file["url"])


def _convert_custom_check_oracle(stream: dict) -> CustomCheckOracle:
    return CustomCheckOracle(
        language=stream["language"],
        function=EvaluationFunction(
            file=stream["file"], name=stream.get("name", "evaluate")
        ),
        arguments=[
            parse_string(v, is_return=True) for v in stream.get("arguments", [])
        ],
    )


def _convert_text_output_channel(
    stream: YamlObject, context: DslContext, config_name: str
) -> TextOutputChannel:
    if isinstance(stream, str):
        data = stream
        config = context.config.get(config_name, {})
        return TextOutputChannel(data=data, oracle=GenericTextOracle(options=config))
    else:
        assert isinstance(stream, dict)
        data = str(stream["data"])
        if "oracle" not in stream or stream["oracle"] == "builtin":
            config = context.deepen_context(
                stream, include_files=False, merge_with=config_name
            ).config
            return TextOutputChannel(
                data=data, oracle=GenericTextOracle(options=config)
            )
        elif stream["oracle"] == "custom_check":
            return TextOutputChannel(
                data=data, oracle=_convert_custom_check_oracle(stream)
            )
        raise TypeError(f"Unknown text oracle type: {stream['oracle']}")


def _convert_advanced_value_output_channel(stream: YamlObject) -> ValueOutputChannel:
    if isinstance(stream, YamlValue):
        # A normal yaml type tagged explicitly.
        value = _convert_value(stream.value)
        assert isinstance(value, Value)
        return ValueOutputChannel(value=value)
    if isinstance(stream, (int, float, bool, TestedType, list, set)):
        # Simple values where no confusion is possible.
        value = _convert_value(stream)
        assert isinstance(value, Value)
        return ValueOutputChannel(value=value)
    elif isinstance(stream, str):
        # A normal YAML string is considered a "Python" string.
        value = parse_string(stream, is_return=True)
        assert isinstance(value, Value)
        return ValueOutputChannel(value=value)
    else:
        # We have an object, which means we have an output channel.
        assert isinstance(stream, dict)
        assert isinstance(stream["value"], str)
        value = parse_string(stream["value"], is_return=True)
        assert isinstance(value, Value)
        if "oracle" not in stream or stream["oracle"] == "builtin":
            return ValueOutputChannel(value=value)
        elif stream["oracle"] == "custom_check":
            return ValueOutputChannel(
                value=value,
                oracle=_convert_custom_check_oracle(stream),
            )
        raise TypeError(f"Unknown value oracle type: {stream['oracle']}")


def _validate_testcase_combinations(testcase: YamlDict):
    if ("stdin" in testcase or "arguments" in testcase) and (
        "statement" in testcase or "expression" in testcase
    ):
        raise ValueError("A main call cannot contain an expression or a statement.")
    if "statement" in testcase and "expression" in testcase:
        raise ValueError("A statement and expression as input are mutually exclusive.")
    if "statement" in testcase and "return" in testcase:
        raise ValueError("A statement cannot have an expected return value.")


def _convert_testcase(testcase: YamlDict, context: DslContext) -> Testcase:
    context = context.deepen_context(testcase)

    # This is backwards compatability to some extend.
    # TODO: remove this at some point.
    if "statement" in testcase and "return" in testcase:
        testcase["expression"] = testcase.pop("statement")

    _validate_testcase_combinations(testcase)
    if (expr_stmt := testcase.get("statement", testcase.get("expression"))) is not None:
        if isinstance(expr_stmt, dict) or context.language != "tested":
            if isinstance(expr_stmt, str):
                the_dict = {context.language: expr_stmt}
            else:
                assert isinstance(expr_stmt, dict)
                the_dict = expr_stmt
            the_dict = {SupportedLanguage(l): cast(str, v) for l, v in the_dict.items()}
            if "statement" in testcase:
                the_type: Literal["statement", "expression"] = "statement"
            else:
                the_type: Literal["statement", "expression"] = "expression"
            the_input = LanguageLiterals(literals=the_dict, type=the_type)
        else:
            assert isinstance(expr_stmt, str)
            the_input = parse_string(expr_stmt)
        return_channel = IgnoredChannel.IGNORED if "statement" in testcase else None
    else:
        if "stdin" in testcase:
            assert isinstance(testcase["stdin"], str)
            stdin = TextData(data=testcase["stdin"])
        else:
            stdin = EmptyChannel.NONE
        arguments = testcase.get("arguments", [])
        assert isinstance(arguments, list)
        the_input = MainInput(stdin=stdin, arguments=arguments)
        return_channel = None

    output = Output()

    if return_channel:
        output.result = return_channel

    if (stdout := testcase.get("stdout")) is not None:
        output.stdout = _convert_text_output_channel(stdout, context, "stdout")
    if (stderr := testcase.get("stderr")) is not None:
        output.stderr = _convert_text_output_channel(stderr, context, "stderr")
    if (exception := testcase.get("exception")) is not None:
        if isinstance(exception, str):
            message = exception
            types = None
        else:
            assert isinstance(exception, dict)
            message = exception.get("message")
            assert isinstance(message, str)
            assert isinstance(exception["types"], dict)
            types = cast(Dict[str, str], exception["types"])
        output.exception = ExceptionOutputChannel(
            exception=ExpectedException(message=message, types=types)
        )
    if (exit_code := testcase.get("exit_code")) is not None:
        output.exit_code = ExitCodeOutputChannel(value=cast(int, exit_code))
    if (result := testcase.get("return")) is not None:
        assert not return_channel
        output.result = _convert_advanced_value_output_channel(result)

    return Testcase(input=the_input, output=output, link_files=context.files)


def _convert_context(context: YamlDict, dsl_context: DslContext) -> Context:
    dsl_context = dsl_context.deepen_context(context)
    raw_testcases = context.get("script", context.get("testcases"))
    assert isinstance(raw_testcases, list)
    testcases = _convert_dsl_list(raw_testcases, dsl_context, _convert_testcase)
    return Context(testcases=testcases)


def _convert_tab(tab: YamlDict, context: DslContext) -> Tab:
    """
    Translate a DSL tab to a full test suite tab.

    :param tab: The tab to translate.
    :param context: The context with config for the parent level.
    :return: A full tab.
    """
    context = context.deepen_context(tab)
    name = tab.get("unit", tab.get("tab"))
    assert isinstance(name, str)

    # The tab can have testcases or contexts.
    if "contexts" in tab:
        assert isinstance(tab["contexts"], list)
        contexts = _convert_dsl_list(tab["contexts"], context, _convert_context)
    elif "cases" in tab:
        assert "unit" in tab
        # We have testcases N.S. / contexts O.S.
        assert isinstance(tab["cases"], list)
        contexts = _convert_dsl_list(tab["cases"], context, _convert_context)
    elif "testcases" in tab:
        # We have scripts N.S. / testcases O.S.
        assert "tab" in tab
        assert isinstance(tab["testcases"], list)
        testcases = _convert_dsl_list(tab["testcases"], context, _convert_testcase)
        contexts = [Context(testcases=[t]) for t in testcases]
    else:
        assert "scripts" in tab
        assert isinstance(tab["scripts"], list)
        testcases = _convert_dsl_list(tab["scripts"], context, _convert_testcase)
        contexts = [Context(testcases=[t]) for t in testcases]

    return Tab(name=name, contexts=contexts)


T = TypeVar("T")


def _convert_dsl_list(
    dsl_list: list, context: DslContext, converter: Callable[[YamlDict, DslContext], T]
) -> List[T]:
    """
    Convert a list of YAML objects into a test suite object.
    """
    objects = []
    for dsl_object in dsl_list:
        assert isinstance(dsl_object, dict)
        objects.append(converter(dsl_object, context))
    return objects


def _convert_dsl(dsl_object: YamlObject) -> Suite:
    """
    Translate a DSL test suite into a full test suite.

    This function assumes the DSL object has been validated;
    errors might not be presented in the best way here.

    :param dsl_object: A validated DSL test suite object.
    :return: A full test suite.
    """
    context = DslContext(config={}, files=[])
    if isinstance(dsl_object, list):
        namespace = None
        tab_list = dsl_object
    else:
        assert isinstance(dsl_object, dict)
        namespace = dsl_object.get("namespace")
        context = context.deepen_context(dsl_object)
        tab_list = dsl_object.get("units", dsl_object.get("tabs"))
        assert isinstance(tab_list, list)
        if (language := dsl_object.get("language", "tested")) != "tested":
            language = SupportedLanguage(language)
        context = evolve(context, language=language)
    tabs = _convert_dsl_list(tab_list, context, _convert_tab)

    if namespace:
        assert isinstance(namespace, str)
        return Suite(tabs=tabs, namespace=namespace)
    else:
        return Suite(tabs=tabs)


def parse_dsl(dsl_string: str) -> Suite:
    """
    Parse a string containing a DSL test suite into our representation,
    a test suite.

    :param dsl_string: The string containing a DSL.
    :return: The parsed and converted test suite.
    """
    dsl_object = _parse_yaml(dsl_string)
    _validate_dsl(dsl_object)
    return _convert_dsl(dsl_object)


def translate_to_test_suite(dsl_string: str) -> str:
    """
    Convert a DSL to a test suite.

    :param dsl_string: The DSL.
    :return: The test suite.
    """
    suite = parse_dsl(dsl_string)
    return suite_to_json(suite)
