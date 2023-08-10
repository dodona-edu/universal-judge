import json
from logging import getLogger
from pathlib import Path
from typing import Callable, Dict, List, Optional, TextIO, TypeVar, Union, cast

import yaml
from jsonschema import Draft7Validator

from tested.datatypes import (
    BasicBooleanTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.dsl.ast_translator import parse_string
from tested.parsing import suite_to_json
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
    MainInput,
    Output,
    Suite,
    Tab,
    Testcase,
    TextData,
    TextOutputChannel,
    ValueOutputChannel,
)
from tested.utils import recursive_dict_merge

logger = getLogger(__name__)

OptionDict = Dict[str, Union[int, bool]]
YamlDict = Dict[str, "YamlObject"]
YamlObject = Union[YamlDict, list, bool, float, int, str, None]


def _parse_yaml(yaml_stream: Union[str, TextIO]) -> YamlObject:
    """
    Parse a string or stream to YAML.
    """
    return yaml.load(yaml_stream, Loader=yaml.CSafeLoader)


def _load_schema_validator():
    """
    Load the JSON Schema validator used to check DSL test suites.
    """
    path_to_schema = Path(__file__).parent / "schema.json"
    with open(path_to_schema, "r") as schema_file:
        schema_object = json.load(schema_file)
    Draft7Validator.check_schema(schema_object)
    return Draft7Validator(schema_object)


_SCHEMA_VALIDATOR = _load_schema_validator()


def _validate_dsl(dsl_object: YamlObject, report=True) -> bool:
    """
    Validate a DSl object.

    :param dsl_object: The object to validate.
    :param report: If errors should be printed or not.
    :return: True if valid, False otherwise.
    """
    errors = list(_SCHEMA_VALIDATOR.iter_errors(dsl_object))
    if not errors:
        return True
    if report:
        for error in errors:
            logger.error(error)

    return False


def _deepen_config_level(
    new_level_object: Optional[YamlDict], current_level: dict
) -> dict:
    """
    Return a dict of options for the new level object.

    This is achieved by taking a copy of the options in the existing level,
    and overriding all options that are present on the new level.

    :param new_level_object: The object from the test suite that may have options.
    :param current_level: The options created for the previous level.

    :return: A dictionary for the next level.
    """
    if new_level_object is None or "config" not in new_level_object:
        return current_level

    assert isinstance(new_level_object["config"], dict)
    return recursive_dict_merge(current_level, new_level_object["config"])


def _convert_value(value: YamlObject) -> Value:
    if value is None:
        return NothingType()
    elif isinstance(value, str):
        return StringType(type=BasicStringTypes.TEXT, data=value)
    elif isinstance(value, bool):
        return BooleanType(type=BasicBooleanTypes.BOOLEAN, data=value)
    elif isinstance(value, int):
        return NumberType(type=BasicNumericTypes.INTEGER, data=value)
    elif isinstance(value, float):
        return NumberType(type=BasicNumericTypes.REAL, data=value)
    elif isinstance(value, list):
        return SequenceType(
            type=BasicSequenceTypes.SEQUENCE,
            data=[_convert_value(part_value) for part_value in value],
        )
    else:
        data = []
        for key, val in value.items():
            data.append(
                ObjectKeyValuePair(
                    key=StringType(type=BasicStringTypes.TEXT, data=key),
                    value=_convert_value(val),
                )
            )
        return ObjectType(type=BasicObjectTypes.MAP, data=data)


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
    stream: YamlObject, config: dict, config_name: str
) -> TextOutputChannel:
    if isinstance(stream, str):
        data = stream
        config = config.get(config_name, {})
        return TextOutputChannel(data=data, oracle=GenericTextOracle(options=config))
    else:
        assert isinstance(stream, dict)
        data = str(stream["data"])
        if "oracle" not in stream or stream["oracle"] == "builtin":
            existing_config = config.get(config_name, {})
            config = _deepen_config_level(stream, existing_config)
            return TextOutputChannel(
                data=data, oracle=GenericTextOracle(options=config)
            )
        elif stream["oracle"] == "custom_check":
            return TextOutputChannel(
                data=data, oracle=_convert_custom_check_oracle(stream)
            )
        raise TypeError(f"Unknown text oracle type: {stream['oracle']}")


def _convert_advanced_value_output_channel(stream: YamlObject) -> ValueOutputChannel:
    if isinstance(stream, str):
        value = parse_string(stream, is_return=True)
        assert isinstance(value, Value)
        return ValueOutputChannel(value=value)
    else:
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


def _convert_testcase(testcase: YamlDict, previous_config: dict) -> Testcase:
    config = _deepen_config_level(testcase, previous_config)

    if (expr_stmt := testcase.get("statement", testcase.get("expression"))) is not None:
        assert isinstance(expr_stmt, str)
        the_input = parse_string(expr_stmt)
    else:
        if "stdin" in testcase:
            assert isinstance(testcase["stdin"], str)
            stdin = TextData(data=testcase["stdin"])
        else:
            stdin = EmptyChannel.NONE
        arguments = testcase.get("arguments", [])
        assert isinstance(arguments, list)
        the_input = MainInput(stdin=stdin, arguments=arguments)

    output = Output()

    if (stdout := testcase.get("stdout")) is not None:
        output.stdout = _convert_text_output_channel(stdout, config, "stdout")
    if (stderr := testcase.get("stderr")) is not None:
        output.stderr = _convert_text_output_channel(stderr, config, "stderr")
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
        if "return_raw" in testcase:
            raise ValueError("Both a return and return_raw value is not allowed.")
        output.result = ValueOutputChannel(value=_convert_value(result))
    if (result := testcase.get("return_raw")) is not None:
        if "return" in testcase:
            raise ValueError("Both a return and return_raw value is not allowed.")
        output.result = _convert_advanced_value_output_channel(result)

    # TODO: allow propagation of files...
    files = []
    if "files" in testcase:
        assert isinstance(testcase["files"], list)
        for yaml_file in testcase["files"]:
            files.append(_convert_file(yaml_file))

    return Testcase(input=the_input, output=output, link_files=files)


def _convert_context(context: YamlDict, previous_config: dict) -> Context:
    config = _deepen_config_level(context, previous_config)
    assert isinstance(context["testcases"], list)
    testcases = _convert_dsl_list(context["testcases"], config, _convert_testcase)
    return Context(testcases=testcases)


def _convert_tab(tab: YamlDict, previous_config: dict) -> Tab:
    """
    Translate a DSL tab to a full test suite tab.

    :param tab: The tab to translate.
    :param previous_config: The config for the parent level.
    :return: A full tab.
    """
    config = _deepen_config_level(tab, previous_config)
    name = tab["tab"]
    assert isinstance(name, str)

    # The tab can have testcases or contexts.
    if "contexts" in tab:
        assert isinstance(tab["contexts"], list)
        contexts = _convert_dsl_list(tab["contexts"], config, _convert_context)
    else:
        assert isinstance(tab["testcases"], list)
        testcases = _convert_dsl_list(tab["testcases"], config, _convert_testcase)
        contexts = [Context(testcases=[t]) for t in testcases]

    return Tab(name=name, contexts=contexts)


T = TypeVar("T")


def _convert_dsl_list(
    dsl_list: list, config: dict, converter: Callable[[YamlDict, dict], T]
) -> List[T]:
    """
    Convert a list of YAML objects into a test suite object.

    :param dsl_list: The YAML list.
    :param config: The config
    :param converter:
    :return:
    """
    objects = []
    for dsl_object in dsl_list:
        assert isinstance(dsl_object, dict)
        objects.append(converter(dsl_object, config))
    return objects


def _convert_dsl(dsl_object: YamlObject) -> Suite:
    """
    Translate a DSL test suite into a full test suite.

    This function assumes the DSL object has been validated;
    errors might not be presented in the best way here.

    :param dsl_object: A validated DSL test suite object.
    :return: A full test suite.
    """
    if isinstance(dsl_object, list):
        namespace = None
        tab_list = dsl_object
        config = {}
    else:
        assert isinstance(dsl_object, dict)
        namespace = dsl_object.get("namespace")
        config = _deepen_config_level(dsl_object, {})
        tab_list = dsl_object["tabs"]
        assert isinstance(tab_list, list)
    tabs = _convert_dsl_list(tab_list, config, _convert_tab)

    if namespace:
        assert isinstance(namespace, str)
        return Suite(tabs=tabs, namespace=namespace)
    else:
        return Suite(tabs=tabs)


def parse_dsl(dsl_string: str, validate: bool = True) -> Suite:
    """
    Parse a string containing a DSL test suite into our representation,
    a test suite.

    :param dsl_string: The string containing a DSL.
    :param validate: If the test suite should be validated or not.
    :return: The parsed and converted test suite.
    """
    dsl_object = _parse_yaml(dsl_string)
    if validate and not _validate_dsl(dsl_object):
        raise ValueError("Cannot parse invalid DSL.")
    return _convert_dsl(dsl_object)


def translate_to_test_suite(dsl_string: str, validate: bool = True) -> str:
    """
    Convert a DSL to a test suite.

    :param dsl_string: The DSL.
    :param validate: Validate the DSL or not.
    :return: The test suite.
    """
    suite = parse_dsl(dsl_string, validate)
    return suite_to_json(suite)
