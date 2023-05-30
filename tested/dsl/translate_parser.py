import json
from logging import getLogger
from pathlib import Path
from typing import Callable, Dict, List, Optional, TextIO, TypeVar, Union

import yaml
from jsonschema import Draft7Validator
from pydantic.json import pydantic_encoder

from tested.datatypes import (
    BasicBooleanTypes,
    BasicNumericTypes,
    BasicObjectTypes,
    BasicSequenceTypes,
    BasicStringTypes,
)
from tested.dsl.ast_translator import parse_string
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
    EmptyChannel,
    ExceptionOutputChannel,
    ExitCodeOutputChannel,
    ExpectedException,
    FileUrl,
    GenericTextEvaluator,
    IgnoredChannel,
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
        # noinspection PyTypeChecker
        for key, val in value.items():
            data.append(
                ObjectKeyValuePair(
                    key=StringType(type=BasicStringTypes.TEXT, data=key),
                    value=_convert_value(val),
                )
            )
        return ObjectType(type=BasicObjectTypes.MAP, data=data)


def _convert_file(link_file: YamlDict) -> FileUrl:
    return FileUrl(name=link_file["name"], url=link_file["url"])


def _convert_text_output_channel(
    stream: YamlObject, config: dict, config_name: str
) -> TextOutputChannel:
    if isinstance(stream, str):
        data = stream
        config = config.get(config_name, {})
    else:
        assert isinstance(stream, dict)
        data = stream["data"]
        existing_config = config.get(config_name, {})
        config = _deepen_config_level(stream, existing_config)

    return TextOutputChannel(data=data, evaluator=GenericTextEvaluator(options=config))


def _convert_testcase(testcase: YamlDict, previous_config: dict) -> Testcase:
    config = _deepen_config_level(testcase, previous_config)

    if (expr_stmt := testcase.get("statement", testcase.get("expression"))) is not None:
        the_input = parse_string(expr_stmt)
        is_statement = testcase.get("statement") is not None
    else:
        stdin = (
            TextData(data=testcase["stdin"])
            if "stdin" in testcase
            else EmptyChannel.NONE
        )
        arguments = testcase.get("arguments", [])
        the_input = MainInput(stdin=stdin, arguments=arguments)
        is_statement = False

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
            message = exception.get("message")
            types = exception["types"]
        output.exception = ExceptionOutputChannel(
            exception=ExpectedException(message=message, types=types)
        )
    if (exit_code := testcase.get("exit_code")) is not None:
        output.exit_code = ExitCodeOutputChannel(exit_code)
    if (result := testcase.get("return")) is not None:
        if "return_raw" in testcase:
            raise ValueError("Both a return and return_raw value is not allowed.")
        output.result = ValueOutputChannel(value=_convert_value(result))
    if (result := testcase.get("return_raw")) is not None:
        if "return" in testcase:
            raise ValueError("Both a return and return_raw value is not allowed.")
        output.result = ValueOutputChannel(value=parse_string(result, True))

    # If there is a return value, allow it.
    if is_statement and not ("return" in testcase or "return_raw" in testcase):
        output.result = IgnoredChannel.IGNORED

    # TODO: allow propagation of files...
    files = []
    if "files" in testcase:
        for yaml_file in testcase["files"]:
            files.append(_convert_file(yaml_file))

    return Testcase(input=the_input, output=output, link_files=files)


def _convert_context(context: YamlDict, previous_config: dict) -> Context:
    config = _deepen_config_level(context, previous_config)
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
    hidden = tab.get("hidden", None)
    name = tab["tab"]

    # The tab can have testcases or contexts.
    if "contexts" in tab:
        contexts = _convert_dsl_list(tab["contexts"], config, _convert_context)
    else:
        assert "testcases" in tab
        testcases = _convert_dsl_list(tab["testcases"], config, _convert_testcase)
        contexts = [Context(testcases=[t]) for t in testcases]

    return Tab(name=name, hidden=hidden, contexts=contexts)


T = TypeVar("T")


def _convert_dsl_list(
    dsl_list: list, config: dict, converter: Callable[[YamlObject, dict], T]
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
    tabs = _convert_dsl_list(tab_list, config, _convert_tab)

    if namespace:
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
    return json.dumps(suite, default=pydantic_encoder, indent=2)
