import dataclasses
import json
import logging
import yaml
from decimal import Decimal

from os import path

from typing import Optional, Any, Union, Callable, Tuple

from tested.datatypes import BasicStringTypes, BasicNumericTypes, \
    BasicBooleanTypes, \
    BasicSequenceTypes, BasicObjectTypes
from tested.serialisation import StringType, NumberType, Value, NothingType, \
    SequenceType, BooleanType, ObjectType, ExceptionValue
from tested.testplan import Plan, Tab, Context, Testcase, TextOutputChannel, \
    Output, \
    ExceptionOutputChannel, ValueOutputChannel, BaseOutput, ContextTestcase, \
    ContextOutput, ExitCodeOutputChannel, ContextInput, TextData, \
    GenericTextEvaluator
from tested.dsl.statement import Parser, ParseError

logger = logging.getLogger(__name__)


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


def valid_text_translatable_type(obj: Any) -> str:
    if not isinstance(obj, (int, float, str, bool)):
        raise TranslateError(f"Invalid text translatable type: {type(obj)}")
    return str(obj)


def ensure_no_null(yaml_obj: dict, key: str) -> Any:
    try:
        value = yaml_obj[key]
        if value is None:
            raise TranslateError(f"Property {key} may not be null")
        return value
    except KeyError as e:
        raise TranslateError(f"Property {key} is not defined") from e


def get_or_none(yaml_obj: dict, key: str) -> Any:
    try:
        value = yaml_obj[key]
        return value
    except KeyError:
        return None


def get_text(stream: Union[Any, dict],
             config: Optional[dict]) -> Tuple[str, dict]:
    if isinstance(stream, dict):
        value = ensure_no_null(stream, 'data')
        config = translate_config(get_or_none(stream, 'config'), config)
    else:
        value = stream
        config = translate_config(config)
    return valid_text_translatable_type(value), config


def translate_file(yaml_file: str, json_file: Optional[str] = None):
    if json_file is None:
        directory = path.dirname(yaml_file)
        json_name = f'{path.splitext(path.basename(yaml_file))[0]}.json'
        json_file = path.join(directory, json_name)
    yaml_str = open(yaml_file, 'r').read()
    json_str = translate(yaml_str)
    if json_str is not None:
        with open(json_file, 'w') as fd:
            print(json_str, file=fd)


def translate(yaml_str: str) -> Optional[str]:
    yaml_obj: list = yaml.safe_load(yaml_str)
    tabs = [
        translate_tab(yaml_tab=tab, index_tab=index)
        for index, tab in enumerate(yaml_obj)
    ]
    if None in tabs:
        logger.error(f"Fault in at least one tab")
        return None
    plan = Plan(tabs=tabs)
    return json.dumps(plan, cls=DataclassJSONEncoder, indent=2)


def translate_common_output(
        yaml_test: dict, config: Optional[dict],
        indices: Union[Tuple[int, int, int], Tuple[int, int]],
        constructor: Callable = Output) -> Optional[BaseOutput]:
    output = constructor()
    fault = False
    try:
        output.stdout = translate_stream(yaml_test['stdout'],
                                         get_or_none(config, 'stdout'))
    except KeyError:
        pass
    except TranslateError as e:
        fault = True
        logger.error(f"Tab {indices[0]}, context {indices[1]}"
                     f"{f'test {indices[2]}' if len(indices) > 2 else ''}: Error "
                     f"in expected stdout: {str(e)}")
    try:
        output.stderr = translate_stream(yaml_test['stderr'],
                                         get_or_none(config, 'stderr'))
    except KeyError:
        pass
    except TranslateError as e:
        fault = True
        logger.error(f"Tab {indices[0]}, context {indices[1]}"
                     f"{f'test {indices[2]}' if len(indices) > 2 else ''}: Error "
                     f"in expected stderr: {str(e)}")
    try:
        output.exception = translate_exception(yaml_test['exception'])
    except KeyError:
        pass
    except TranslateError as e:
        fault = True
        logger.error(f"Tab {indices[0]}, context {indices[1]}"
                     f"{f'test {indices[2]}' if len(indices) > 2 else ''}: Error "
                     f"in expected exception message: {str(e)}")
    return output if not fault else None


def translate_config(new_config: Optional[dict] = None,
                     old_config: Optional[dict] = None) -> dict:
    def merge(dict0, dict1):
        merged_dict = dict()
        for key in set(dict0.keys()).union(set(dict1.keys())):
            if key in dict0 and key in dict1:
                # Both dictionaries contains the key
                if isinstance(dict0[key], dict) and isinstance(dict1[key], dict):
                    # Merge subsequence dictionaries
                    merged_dict[key] = merge(dict0[key], dict1[key])
                else:
                    # Non subsequence dictionaries, keep value of dict1
                    merged_dict[key] = dict1[key]
            # Just copy unique keys
            elif key in dict0:
                merged_dict[key] = dict0[key]
            else:
                merged_dict[key] = dict1[key]

        return merged_dict

    # Fast exit if possible
    if old_config is None:
        if new_config is None:
            return {}
        else:
            return new_config
    if new_config is None:
        return old_config
    # Merge config dictionaries
    return merge(old_config, new_config)


def translate_ctx(yaml_context, indices: Tuple[int, int],
                  config: Optional[dict]) -> Optional[Context]:
    config = translate_config(get_or_none(yaml_context, 'config'), config)
    try:
        testcases = [translate_test(test, config, (indices[0], indices[1], index))
                     for index, test in enumerate(yaml_context['tests'])]
    except KeyError:
        testcases = []
    ctx_testcase = translate_ctx_test(yaml_context, config, indices)
    if testcases is None or ctx_testcase is None:
        logger.error(
            f"At least one fault in context {indices[1]} at tab {indices[0]}")
        return None
    return Context(context_testcase=ctx_testcase, testcases=testcases)


def translate_ctx_input(yaml_context: dict,
                        indices: Tuple[int, int]) -> Optional[ContextInput]:
    ctx_input = ContextInput()
    fault = False
    try:
        ctx_input.arguments = [valid_text_translatable_type(val) for val in
                               yaml_context['arguments']]
        ctx_input.main_call = True
    except KeyError:
        pass
    except TranslateError as e:
        logger.error(f"Tab {indices[0]}, context {indices[1]}: Invalid argument: "
                     f"{str(e)}")
        fault = True
    try:
        ctx_input.stdin = TextData(
            data=valid_text_translatable_type(yaml_context['stdin']))
        ctx_input.main_call = True
    except KeyError:
        pass
    except TranslateError as e:
        logger.error(f"Tab {indices[0]}, context {indices[1]}: Invalid stdin: "
                     f"{str(e)}")
        fault = True
    if fault:
        return None
    return ctx_input


def translate_ctx_test(yaml_context: dict,
                       config: Optional[dict],
                       indices: Tuple[int, int]) -> Optional[ContextTestcase]:
    output = translate_common_output(yaml_context, config, indices,
                                     constructor=ContextOutput)
    ctx_input = translate_ctx_input(yaml_context, indices)
    try:
        exit_code = yaml_context["exit_code"]
        if not isinstance(exit_code, int):
            return None
        if output is None or ctx_input is None:
            return None
        output.exit_code = ExitCodeOutputChannel(value=exit_code)
        ctx_input.main_call = True
    except KeyError:
        pass
    if output is None or ctx_input is None:
        return None
    ctx_input.main_call = (ctx_input.main_call or
                           isinstance(output.stdout, TextOutputChannel) or
                           isinstance(output.stderr, TextOutputChannel) or
                           isinstance(output.exception, ExceptionOutputChannel))
    return ContextTestcase(output=output, input=ctx_input)


def translate_exception(stream: str) -> ExceptionOutputChannel:
    return ExceptionOutputChannel(
        exception=ExceptionValue(message=valid_text_translatable_type(stream)))


def translate_stream(
        stream: Union[str, int, float, bool, dict],
        config: Optional[dict]
) -> TextOutputChannel:
    value, config = get_text(stream, config)
    return TextOutputChannel(data=value,
                             evaluator=GenericTextEvaluator(options=config))


def translate_tab(yaml_tab: dict, index_tab: int) -> Optional[Tab]:
    config = translate_config(get_or_none(yaml_tab, 'config'))
    contexts = [
        translate_ctx(ctx, (index_tab, index), config)
        for index, ctx in enumerate(ensure_no_null(yaml_tab, 'testcases'))
    ]
    if None in contexts:
        logger.error(f"At least one context fault in tab {index_tab}")
        return None
    return Tab(name=ensure_no_null(yaml_tab, 'tab'), contexts=contexts)


def translate_test(yaml_test: dict, config: Optional[dict],
                   indices: Tuple[int, int, int]) -> Optional[Testcase]:
    fault = False
    try:
        stmt = parser.parse_statement(
            valid_text_translatable_type(ensure_no_null(yaml_test, 'statement')))
    except (ParseError, TranslateError) as e:
        logger.error(f"Tab {indices[0]}, context {indices[1]}, test {indices[2]}: "
                     f"Invalid statement: {str(e)}")
        fault = True
    output = translate_common_output(yaml_test, config, indices)
    try:
        result = translate_value_output(yaml_test['return'], indices)
        if output is not None:
            output.result = result
        else:
            fault = True
    except KeyError:
        try:
            value = parser.parse_value(
                valid_text_translatable_type(yaml_test['return-raw']))
            output.result = ValueOutputChannel(value=value)
        except KeyError:
            pass
        except (ParseError, TranslateError) as e:
            logger.error(
                f"Tab {indices[0]}, context {indices[1]}, test {indices[2]}: "
                f"Invalid raw value: {str(e)}")
            fault = True
    if fault:
        return None
    return Testcase(input=stmt, output=output)


def translate_value_output(value: Optional[
    Union[str, int, bool, float, list, dict]
], indices: Tuple[int, int, int]) -> Optional[ValueOutputChannel]:
    def parse_value(partial_value: Optional[
        Union[str, int, bool, float, list, dict]
    ], level=0) -> Value:
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
                parse_value(part_value, level=level + 1)
                for part_value in partial_value
            ])
        elif isinstance(partial_value, dict):
            return ObjectType(type=BasicObjectTypes.MAP, data=dict(
                (key, parse_value(val, level=level + 1))
                for key, val in partial_value.items()
            ))
        else:
            raise TranslateError(
                f"Invalid value type at level {level}: {type(partial_value)}")

    try:
        return ValueOutputChannel(value=parse_value(value))
    except TranslateError as e:
        logger.error(f"Tab {indices[0]}, context {indices[1]}, test {indices[2]}: "
                     f"Value error: {str(e)}")
        return None
