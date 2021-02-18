from copy import deepcopy
from dataclasses import dataclass, field
from logging import getLogger
from itertools import groupby
from json import dumps
from jsonschema import Draft7Validator
from pydantic.json import pydantic_encoder
from os.path import basename, dirname, join, splitext
from typing import Any, Dict, List, Optional, Union, Tuple, Callable
from yaml import safe_load

from tested.datatypes import BasicBooleanTypes, BasicNumericTypes, \
    BasicObjectTypes, BasicSequenceTypes, BasicStringTypes
from tested.dsl.statement import Parser
from tested.serialisation import ExceptionValue, Value, NothingType, StringType, \
    BooleanType, NumberType, SequenceType, ObjectType
from tested.testplan import BaseOutput, Context, EmptyChannel, \
    ExceptionOutputChannel, FileUrl, GenericTextEvaluator, Output, Plan, Run, \
    RunTestcase, RunInput, RunOutput, Tab, Testcase, TextData, TextOutputChannel, \
    ValueOutputChannel, ExitCodeOutputChannel

logger = getLogger(__name__)

YAML_DICT = Dict[str, 'YAML_OBJECT']
YAML_LIST = list
YAML_OBJECT = Union[YAML_DICT, YAML_LIST, bool, float, int, str, None]

OPTION_DICT = Dict[str, Union[int, bool]]

parser = Parser()


@dataclass
class StackFrame:
    options_stdout: OPTION_DICT = field(default_factory=dict)
    """Options for processing standard output"""
    options_stderr: OPTION_DICT = field(default_factory=dict)
    """Options for processing standard error"""
    options_exception: OPTION_DICT = field(default_factory=dict)
    """Options for processing exception"""
    options_exit_code: OPTION_DICT = field(default_factory=dict)
    """Options for processing exit codes"""
    options_return: OPTION_DICT = field(default_factory=dict)
    """Options for processing return values"""
    link_files: List[FileUrl] = field(default_factory=list)
    """Collect files to link in feedback"""


class SchemaParser:
    def __init__(self, schema_path: Optional[str] = None, indent: int = 2):
        if schema_path is None:
            schema_path = join(dirname(__file__), "schema.yaml")
        schema_str = open(schema_path, 'r').read()
        schema = self._load_yaml_object(schema_str)
        Draft7Validator.check_schema(schema)
        self.validator = Draft7Validator(schema)
        self.indent = indent

    def translate(self, yaml_file: str, json_file: Optional[str] = None):
        yaml_str = open(yaml_file, 'r').read()
        json_str = self.translate_str(yaml_str)
        if json_file is None:
            directory = dirname(yaml_file)
            json_name = f'{splitext(basename(yaml_file))[0]}.json'
            json_file = join(directory, json_name)
        with open(json_file, 'w') as fd:
            print(json_str, file=fd)

    def translate_str(self, yaml_str: str) -> str:
        plan = self.load_str(yaml_str)
        json_str = self._write_to_json_string(plan)
        return json_str

    def load(self, yaml_file: str) -> Plan:
        yaml_str = open(yaml_file, 'r').read()
        return self.load_str(yaml_str)

    def load_str(self, yaml_str: str) -> Plan:
        yaml_obj = self._load_yaml_object(yaml_str)
        self._validate(yaml_obj)
        return self._translate_plan(yaml_obj)

    # noinspection PyMethodMayBeStatic
    def _enforce_dict(self, yaml_obj: YAML_OBJECT) -> YAML_DICT:
        if isinstance(yaml_obj, dict):
            return yaml_obj
        else:
            raise Exception(
                f"Invalid datatype {type(yaml_obj)}: dictionary expected"
            )

    # noinspection PyMethodMayBeStatic
    def _enforce_list(self, yaml_obj: YAML_OBJECT) -> YAML_LIST:
        if isinstance(yaml_obj, list):
            return yaml_obj
        else:
            raise Exception(f"Invalid datatype {type(yaml_obj)}: list expected")

    # noinspection PyMethodMayBeStatic
    def _get_any_safe(self, yaml_dict: Any, key: str) -> Optional[Any]:
        try:
            return yaml_dict[key]
        except KeyError:
            return None

    # noinspection PyMethodMayBeStatic
    def _get_bool_safe(self, yaml_dict: YAML_DICT, key: str) -> Optional[bool]:
        try:
            return bool(yaml_dict[key])
        except KeyError:
            return None

    # noinspection PyMethodMayBeStatic
    def _get_int_safe(self, yaml_dict: YAML_DICT, key: str) -> Optional[int]:
        try:
            x = yaml_dict[key]
            if isinstance(x, int):
                return x
            return None
        except KeyError:
            return None

    def _get_dict_safe(self, yaml_dict: YAML_DICT, key: str) -> Optional[YAML_DICT]:
        try:
            x = yaml_dict[key]
            self._enforce_dict(x)
            return x
        except KeyError:
            return None

    def _get_list_safe(self, yaml_dict: YAML_DICT, key: str) -> Optional[YAML_LIST]:
        try:
            x = yaml_dict[key]
            self._enforce_list(x)
            return x
        except KeyError:
            return None

    # noinspection PyMethodMayBeStatic
    def _get_str_safe(self, yaml_dict: YAML_DICT, key: str) -> Optional[str]:
        try:
            return str(yaml_dict[key])
        except KeyError:
            return None

    # noinspection PyMethodMayBeStatic
    def _get_str_dict_safe(self,
                           yaml_dict: YAML_DICT,
                           key: str) -> Optional[Union[str, OPTION_DICT]]:
        try:
            x = yaml_dict[key]
            if isinstance(x, dict):
                return x
            return str(x)
        except KeyError:
            return None

    # noinspection PyMethodMayBeStatic
    def _get_int_dict_safe(self,
                           yaml_dict: YAML_DICT,
                           key: str) -> Optional[Union[str, OPTION_DICT]]:
        try:
            x = yaml_dict[key]
            if isinstance(x, (int, dict)):
                return x
            return None
        except KeyError:
            return None

    def _get_str_and_hide_expected(self,
                                   stack_frame: StackFrame,
                                   yaml_object: YAML_OBJECT,
                                   extract_config: Callable[[StackFrame], YAML_DICT]
                                   ) -> Tuple[str, bool]:
        if isinstance(yaml_object, str):
            data = yaml_object
            hide_expected = self._get_bool_safe(extract_config(stack_frame),
                                                "hide_expected") or False
        else:
            data = self._get_str_safe(yaml_object, "data")
            hide_expected = self._get_bool_safe(
                merge(extract_config(stack_frame),
                      self._get_dict_safe(yaml_object, "config")),
                "hide_expected") or False
        return data, hide_expected

    # noinspection PyMethodMayBeStatic
    def _load_yaml_object(self, yaml_str: str) -> YAML_OBJECT:
        return safe_load(yaml_str)

    def _translate_base_output(self,
                               yaml_dict: YAML_DICT,
                               output: BaseOutput,
                               stack_frame: StackFrame = StackFrame()):
        if "exception" in yaml_dict:
            output.exception = self._translate_exception(
                stack_frame,
                self._get_str_dict_safe(yaml_dict, "exception")
            )
        if "stderr" in yaml_dict:
            output.stderr = self._translate_stream(
                self._get_str_dict_safe(yaml_dict, "stderr"),
                stack_frame.options_stderr
            )
        if "stdout" in yaml_dict:
            output.stdout = self._translate_stream(
                self._get_str_dict_safe(yaml_dict, "stdout"),
                stack_frame.options_stdout
            )

    # noinspection PyMethodMayBeStatic
    def _translate_config(
            self,
            config: Optional[YAML_DICT] = None,
            old_stack_frame: StackFrame = StackFrame()
    ) -> StackFrame:
        new_stack_frame = StackFrame()
        if config and "stdout" in config:
            new_stack_frame.options_stdout = merge(
                old_stack_frame.options_stdout,
                self._get_dict_safe(config, "stdout")
            )
        else:
            new_stack_frame.options_stdout = old_stack_frame.options_stdout
        if config and "stderr" in config:
            new_stack_frame.options_stderr = merge(
                old_stack_frame.options_stderr,
                self._get_dict_safe(config, "stderr")
            )
        else:
            new_stack_frame.options_stderr = old_stack_frame.options_stderr
        if config and "exit_code" in config:
            new_stack_frame.options_exit_code = merge(
                old_stack_frame.options_exit_code,
                self._get_dict_safe(config, "exit_code")
            )
        else:
            new_stack_frame.options_exit_code = old_stack_frame.options_exit_code
        if config and "exception" in config:
            new_stack_frame.options_exception = merge(
                old_stack_frame.options_exception,
                self._get_dict_safe(config, "exception")
            )
        else:
            new_stack_frame.options_exception = old_stack_frame.options_exception
        if config and "return" in config:
            new_stack_frame.options_return = merge(
                old_stack_frame.options_return,
                self._get_dict_safe(config, "return")
            )
        else:
            new_stack_frame.options_return = old_stack_frame.options_return
        new_stack_frame.link_files = deepcopy(old_stack_frame.link_files)
        return new_stack_frame

    def _translate_context(self,
                           context: YAML_DICT,
                           stack_frame: StackFrame = StackFrame()
                           ) -> Tuple[RunTestcase, Optional[Context]]:
        if "statement" in context or "expression" in context:
            testcase, files = self._translate_testcase(context, stack_frame)
            return RunTestcase(), Context(testcases=[testcase], link_files=files)

        stack_frame = self._translate_config(
            self._get_dict_safe(context, "config"),
            stack_frame
        )
        run_testcase = self._translate_context_testcase(context, stack_frame)

        testcases = []

        if "testcases" in context:
            for testcase, files in (self._translate_testcase(testcase, stack_frame)
                                    for testcase in
                                    self._get_list_safe(context, "testcases")):
                testcases.append(testcase)
                stack_frame.link_files.extend(files)

        if "files" in context:
            stack_frame.link_files.extend(
                self._translate_file(file)
                for file in self._get_list_safe(context, "files")
            )

        if len(testcases) == 0:
            return run_testcase, None

        unique_file_urls = list(
            k for k, _ in groupby(sorted(stack_frame.link_files,
                                         key=lambda x: (x.name, x.content))))
        return run_testcase, Context(testcases=testcases,
                                     link_files=unique_file_urls)

    def _translate_file(self, link_file: YAML_DICT) -> FileUrl:
        name = self._get_str_safe(link_file, "name")
        url = self._get_str_safe(link_file, "url")
        return FileUrl(name=name, content=url)

    def _translate_plan(self, yaml_obj: YAML_OBJECT) -> Plan:
        if isinstance(yaml_obj, list):
            tabs = [
                self._translate_tab(self._enforce_dict(yaml_obj))
                for yaml_obj in yaml_obj
            ]
        else:
            tabs = []
        return Plan(tabs=tabs)

    def _translate_context_testcase(
            self,
            context: YAML_DICT,
            stack_frame: StackFrame = StackFrame()
    ) -> RunTestcase:
        main_call = (
                "arguments" in context or "exception" in context or
                "exit_code" in context or "stdin" in context or
                "stdout" in context or "stderr" in context
        )

        if "arguments" in context:
            arguments = [
                str(argument)
                for argument in self._get_list_safe(context, "arguments")
            ]
        else:
            arguments = []
        if "stdin" in context:
            stdin = TextData(data=self._get_str_safe(context, "stdin"))
        else:
            stdin = EmptyChannel.NONE

        run_input = RunInput(stdin=stdin, arguments=arguments, main_call=main_call)

        run_output = RunOutput()
        if "exit_code" in context:
            run_output.exit_code = self._translate_exit_code(
                stack_frame,
                self._get_int_dict_safe(context, "exit_code")
            )

        self._translate_base_output(context, run_output, stack_frame)
        return RunTestcase(input=run_input, output=run_output)

    def _translate_stream(self,
                          stream: YAML_OBJECT,
                          config: OPTION_DICT) -> TextOutputChannel:
        if isinstance(stream, str):
            data, config = stream, config
        else:
            data = self._get_str_safe(stream, "data")
            config = merge(config, self._get_dict_safe(stream, "config"))

        hide_expected = self._get_bool_safe(config, "hide_expected") or False

        return TextOutputChannel(data=data,
                                 evaluator=GenericTextEvaluator(options=config),
                                 show_expected=not hide_expected)

    def _translate_exit_code(self,
                             stack_frame: StackFrame,
                             exit_code: YAML_OBJECT) -> ExitCodeOutputChannel:
        if isinstance(exit_code, int):
            exit_code: int
            hide_expected = self._get_bool_safe(stack_frame.options_exit_code,
                                                "hide_expected") or False
        else:
            exit_code: YAML_DICT
            hide_expected = self._get_bool_safe(
                merge(stack_frame.options_exit_code,
                      self._get_dict_safe(exit_code, "config")),
                "hide_expected"
            ) or False
            exit_code: int = self._get_bool_safe(exit_code, "data") or 0
        return ExitCodeOutputChannel(value=exit_code,
                                     show_expected=not hide_expected)

    def _translate_exception(self,
                             stack_frame: StackFrame,
                             exception: YAML_OBJECT) -> ExceptionOutputChannel:
        data, hide_expected = self._get_str_and_hide_expected(
            stack_frame,
            exception,
            lambda x: x.options_exception
        )

        return ExceptionOutputChannel(
            exception=ExceptionValue(
                message=data
            ),
            show_expected=not hide_expected
        )

    def _translate_tab(self,
                       tab: YAML_DICT,
                       stack_frame: StackFrame = StackFrame()) -> Tab:
        stack_frame = self._translate_config(
            self._get_dict_safe(tab, "config"),
            stack_frame
        )
        hidden = self._get_bool_safe(tab, "hidden")
        name = self._get_str_safe(tab, "tab")
        optimize = self._get_bool_safe(tab, "disable_optimizations") is not True

        translated_contexts = [
            self._translate_context(context, stack_frame)
            for context in self._get_list_safe(tab, "contexts")
        ]

        if len(translated_contexts) == 0:
            return Tab(name=name, hidden=hidden, runs=[])

        runs = []
        if optimize:
            new_run = False
            for context_testcase, context in translated_contexts:
                if context_testcase.input.main_call:
                    runs.append(Run(run=context_testcase))
                    if context_testcase.output.exit_code.value != 0:
                        new_run = True
                if context is not None:
                    if new_run:
                        new_run = False
                        runs.append(Run(contexts=[context]))
                    else:
                        try:
                            runs[-1].contexts.append(context)
                        except IndexError:
                            runs.append(Run(contexts=[context]))
        else:
            runs = [
                Run(contexts=[context] if context is not None else [],
                    run=context_testcase)
                for context_testcase, context in translated_contexts
            ]

        return Tab(name=name, hidden=hidden, runs=runs)

    def _translate_testcase(self,
                            testcase: YAML_DICT,
                            stack_frame: StackFrame = StackFrame()
                            ) -> Tuple[Testcase, List[FileUrl]]:
        stack_frame = self._translate_config(
            self._get_dict_safe(testcase, "config"),
            stack_frame
        )
        code = (self._get_str_safe(testcase, "statement") or
                self._get_str_safe(testcase, "expression"))
        output = Output()
        self._translate_base_output(testcase, output, stack_frame)
        if "return" in testcase:
            hide_expected = self._get_bool_safe(stack_frame.options_return,
                                                "hide_expected") or False
            output.result = ValueOutputChannel(
                value=self._translate_value(
                    self._get_any_safe(testcase, "return")),
                show_expected=not hide_expected
            )
        elif "return-raw" in testcase:
            output.result = self._translate_raw_return(
                stack_frame,
                self._get_str_dict_safe(testcase, "return-raw")
            )

        testcase_value = Testcase(input=parser.parse_statement(code), output=output)

        if "files" in testcase:
            files = [
                self._translate_file(file)
                for file in self._get_list_safe(testcase, "files")
            ]
        else:
            files = []
        return testcase_value, files

    def _translate_raw_return(self, stack_frame: StackFrame,
                              value: YAML_OBJECT) -> ValueOutputChannel:
        data, hide_expected = self._get_str_and_hide_expected(
            stack_frame,
            value,
            lambda x: x.options_return
        )
        return ValueOutputChannel(
            value=parser.parse_value(data),
            show_expected=not hide_expected
        )

    def _translate_value(self, value: YAML_DICT) -> Value:
        if value is None:
            return NothingType()
        elif isinstance(value, str):
            return StringType(type=BasicStringTypes.TEXT, data=value)
        elif isinstance(value, bool):
            return BooleanType(type=BasicBooleanTypes.BOOLEAN, data=value)
        elif isinstance(value, int):
            return NumberType(type=BasicNumericTypes.INTEGER, data=value)
        elif isinstance(value, float):
            return NumberType(type=BasicNumericTypes.RATIONAL, data=value)
        elif isinstance(value, list):
            return SequenceType(type=BasicSequenceTypes.SEQUENCE, data=[
                self._translate_value(part_value) for part_value in value
            ])
        else:
            return ObjectType(type=BasicObjectTypes.MAP, data=dict(
                (key, self._translate_value(val)) for key, val in value.items()
            ))

    def _write_to_json_string(self, json_object: Any) -> str:
        return dumps(json_object, default=pydantic_encoder, indent=self.indent)

    def _validate(self, instance: YAML_OBJECT):
        if not self.validator.is_valid(instance):
            for error in self.validator.iter_errors(instance):
                logger.error(error.message)
            exit(1)


def merge(dict0: YAML_DICT, dict1: YAML_DICT) -> YAML_DICT:
    merged_dict = dict()
    for key in set(dict0.keys()).union(set(dict1.keys())):
        if key in dict0 and key in dict1:
            # Both dictionaries contains the key
            if isinstance(dict0[key], dict) and isinstance(dict1[key],
                                                           dict):
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
