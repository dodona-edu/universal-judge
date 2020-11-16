import dataclasses
import json
from decimal import Decimal

import yaml

from os import path

from typing import Optional, Any, Union, Callable

from tested.datatypes import BasicStringTypes, BasicNumericTypes, \
    BasicBooleanTypes, \
    BasicSequenceTypes, BasicObjectTypes
from tested.serialisation import StringType, NumberType, Value, NothingType, \
    SequenceType, BooleanType, ObjectType
from tested.testplan import Plan, Tab, Context, Testcase, TextOutputChannel, \
    Output, \
    ExceptionOutputChannel, ValueOutputChannel, BaseOutput, ContextTestcase, \
    ContextOutput, ExitCodeOutputChannel, ContextInput, TextData
from tested.dsl.statement import Parser


# Idea: Stackoverflow:
# https://stackoverflow.com/questions/51286748/make-the-python-json-encoder
# -support-pythons-new-dataclasses#answer-51286749
class DataclassJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if dataclasses.is_dataclass(o):
            return dataclasses.asdict(o)
        if isinstance(o, Decimal):
            if o == o.to_integral_value():
                return int(o)
            else:
                return float(o)

        return super().default(o)


class TranslateError(Exception):
    pass


parser = Parser()


def ensure_no_null(yaml_obj: dict, key: str) -> Any:
    try:
        value = yaml_obj[key]
        if value is None:
            raise TranslateError(f"Property {key} may not be null")
        return value
    except KeyError as e:
        raise TranslateError(f"Property {key} is not defined") from e


def translate_file(yaml_file: str, json_file: Optional[str] = None):
    if json_file is None:
        directory = path.dirname(yaml_file)
        json_name = f'{path.splitext(path.basename(yaml_file))[0]}.json'
        json_file = path.join(directory, json_name)
    yaml_str = open(yaml_file, 'r').read()
    json_str = translate(yaml_str)
    with open(json_file, 'w') as fd:
        print(json_str, file=fd)


def translate(yaml_str: str) -> str:
    yaml_obj = yaml.safe_load(yaml_str)
    plan = Plan(tabs=[translate_tab(yaml_tab=tab) for tab in yaml_obj])
    return json.dumps(plan, cls=DataclassJSONEncoder, indent=2)


def translate_common_output(yaml_test: dict, config,
                            constructor: Callable = Output) -> BaseOutput():
    output = constructor()
    try:
        output.stdout = translate_stream(yaml_test['stdout'], config)
    except KeyError:
        pass
    try:
        output.stderr = translate_stream(yaml_test['stderr'], config)
    except KeyError:
        pass
    try:
        output.exception = translate_stream(yaml_test['exception'], config)
    except KeyError:
        pass
    return output


def translate_value_output(value: Optional[
    Union[str, int, bool, float, list, dict]
]) -> ValueOutputChannel:
    def parse_value(partial_value: Optional[
        Union[str, int, bool, float, list, dict]
    ]) -> Value:
        if partial_value is None:
            return NothingType()
        elif isinstance(partial_value, str):
            return StringType(type=BasicStringTypes.TEXT, data=partial_value)
        elif isinstance(partial_value, bool):
            return BooleanType(type=BasicBooleanTypes.BOOLEAN, data=partial_value)
        elif isinstance(partial_value, int):
            return NumberType(type=BasicNumericTypes.INTEGER, data=partial_value)
        elif isinstance(partial_value, float):
            return NumberType(type=BasicNumericTypes.RATIONAL, data=partial_value)
        elif isinstance(partial_value, list):
            return SequenceType(type=BasicSequenceTypes.SEQUENCE, data=[
                parse_value(part_value) for part_value in partial_value
            ])
        elif isinstance(partial_value, dict):
            return ObjectType(type=BasicObjectTypes.MAP, data=dict(
                (key, parse_value(val)) for key, val in partial_value.items()
            ))

    return ValueOutputChannel(value=parse_value(value))


def translate_ctx(yaml_context, config) -> Context:
    try:
        testcases = [translate_test(test, config) for test in yaml_context['tests']]
    except KeyError as e:
        testcases = []
    ctx_testcase = translate_ctx_test(yaml_context, config)
    return Context(context_testcase=ctx_testcase, testcases=testcases)


def translate_ctx_input(yaml_context: dict) -> ContextInput:
    ctx_input = ContextInput()
    try:
        ctx_input.arguments = [str(val) for val in yaml_context['arguments']]
        ctx_input.main_call = True
    except KeyError:
        pass
    try:
        ctx_input.stdin = TextData(data=str(yaml_context['stdin']))
        ctx_input.main_call = True
    except KeyError:
        pass
    return ctx_input


def translate_ctx_test(yaml_context: dict, config) -> ContextTestcase:
    output = translate_common_output(yaml_context, config,
                                     constructor=ContextOutput)
    ctx_input = translate_ctx_input(yaml_context)
    ctx_input.main_call = (ctx_input.main_call or
                           isinstance(output.stdout, TextOutputChannel) or
                           isinstance(output.stderr, TextOutputChannel) or
                           isinstance(output.exception, ExceptionOutputChannel))

    try:
        exit_code = yaml_context["exit_code"]
        output.exit_code = ExitCodeOutputChannel(value=exit_code)
        ctx_input.main_call = True
    except KeyError:
        pass
    return ContextTestcase(output=output, input=ctx_input)


def translate_exception(stream: Union[str, dict], config) -> ExceptionOutputChannel:
    if isinstance(stream, str):
        return ExceptionOutputChannel(exception=stream)
    raise TranslateError("Failure")


def translate_stream(stream: Union[str, dict], config) -> TextOutputChannel:
    if isinstance(stream, str):
        return TextOutputChannel(data=stream)
    raise TranslateError("Failure")


def translate_tab(yaml_tab: dict) -> Tab:
    config = {}
    return Tab(name=ensure_no_null(yaml_tab, 'tab'),
               contexts=[translate_ctx(ctx, config) for ctx in
                         ensure_no_null(yaml_tab, 'testcases')])


def translate_test(yaml_test: dict, config) -> Testcase:
    stmt = parser.parse_statement(ensure_no_null(yaml_test, 'statement'))
    output = translate_common_output(yaml_test, config)
    try:
        output.result = translate_value_output(yaml_test['return'])
    except KeyError:
        pass
    return Testcase(input=stmt, output=output)
