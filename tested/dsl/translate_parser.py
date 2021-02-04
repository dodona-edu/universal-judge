from copy import deepcopy
from dataclasses import dataclass, field
from logging import getLogger
from json import dumps
from jsonschema import Draft7Validator
from os.path import basename, dirname, join, splitext
from typing import Any, Dict, List, Optional, Union
from yaml import safe_load

from tested.datatypes import BasicBooleanTypes, BasicNumericTypes, \
    BasicObjectTypes, BasicSequenceTypes, BasicStringTypes
from tested.dsl.statement import Parser, ParseError
from tested.serialisation import ExceptionValue, Value, NothingType, StringType, \
    BooleanType, NumberType, SequenceType, ObjectType
from tested.testplan import BaseOutput, Context, ExceptionOutputChannel, FileUrl, \
    GenericTextEvaluator, Plan, Run, RunTestcase, Tab, Testcase, \
    TextOutputChannel, ValueOutputChannel

from .JSONEncoder import DataclassJSONEncoder

logger = getLogger(__name__)

YAML_DICT = Dict[str, 'YAML_OBJECT']
YAML_LIST = list
YAML_OBJECT = Union[YAML_DICT, YAML_LIST, bool, float, int, str, None]

OPTION_DICT = Dict[str, Union[int, bool]]

parser = Parser()


@dataclass
class StackFrame:
    """Options for processing standard output"""
    options_stdout: OPTION_DICT = field(default_factory=dict)
    """Options for processing standard error"""
    options_stderr: OPTION_DICT = field(default_factory=dict)
    """Collect files to link in feedback"""
    link_files: List[FileUrl] = field(default_factory=list)


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
        yaml_obj = self._load_yaml_object(yaml_str)
        self._validate(yaml_obj)
        plan = self._translate_plan(yaml_obj)
        return self._write_to_json_string(plan)

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
            return int(yaml_dict[key])
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
            if isinstance(x, (str, dict)):
                return x
            return None
        except KeyError:
            return None

    # noinspection PyMethodMayBeStatic
    def _load_yaml_object(self, yaml_str: str) -> YAML_OBJECT:
        return safe_load(yaml_str)

    def _translate_base_output(self,
                               yaml_dict: YAML_DICT,
                               output: BaseOutput,
                               stack_frame: StackFrame = StackFrame()):
        if "exception" in yaml_dict:
            output.exception = ExceptionOutputChannel(
                exception=ExceptionValue(
                    message=self._get_str_safe(yaml_dict, "exception")
                )
            )
        if "stderr" in yaml_dict:
            output.stderr = self._translate_stream(
                self._get_str_dict_safe(yaml_dict, "stderr"),
                stack_frame.options_stderr
            )
        if "stdout" in yaml_dict:
            output.stderr = self._translate_stream(
                self._get_str_dict_safe(yaml_dict, "stdout"),
                stack_frame.options_stdout
            )

    # noinspection PyMethodMayBeStatic
    def _translate_config(
            self,
            config: Optional[YAML_DICT] = None,
            old_stack_frame: StackFrame = StackFrame()
    ) -> StackFrame:
        if config is None:
            return old_stack_frame
        new_stack_frame = StackFrame()
        new_stack_frame.options_stdout = merge(config,
                                               old_stack_frame.options_stdout)
        new_stack_frame.options_stderr = merge(config,
                                               old_stack_frame.options_stderr)
        new_stack_frame.link_files = deepcopy(old_stack_frame.link_files)
        return new_stack_frame

    def _translate_context(self,
                           context: YAML_DICT,
                           stack_frame: StackFrame = StackFrame()) -> Context:
        stack_frame = self._translate_config(
            self._get_dict_safe(context, "config"),
            stack_frame
        )
        testcases = [
            self._translate_testcase(testcase, stack_frame)
            for testcase in self._get_list_safe(context, "testcases")
        ]
        if "files" in context:
            stack_frame.link_files.extend(
                self._translate_file(file)
                for file in self._get_list_safe(context, "files")
            )
        return Context(testcases=testcases, link_files=stack_frame.link_files)

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

    def _translate_run(self, run: YAML_DICT,
                       stack_frame: StackFrame = StackFrame()) -> Run:
        stack_frame = self._translate_config(
            self._get_dict_safe(run, "config"),
            stack_frame
        )
        if "files" in run:
            stack_frame.link_files.extend(
                self._translate_file(file)
                for file in self._get_list_safe(run, "files")
            )
        if "contexts" in run:
            contexts = [
                self._translate_context(context, stack_frame)
                for context in self._get_list_safe(run, "contexts")
            ]
        else:
            contexts = []
        if "run" in run:
            run_testcase = self._translate_run_testcase(
                self._get_dict_safe(run, "run"), stack_frame)
        else:
            run_testcase = RunTestcase()
        return Run(contexts=contexts, run=run_testcase)

    def _translate_run_testcase(
            self,
            run_testcase: YAML_DICT,
            stack_frame: StackFrame = StackFrame()
    ) -> RunTestcase:
        run = RunTestcase()
        run.input.main_call = True

        if "arguments" in run_testcase:
            run.input.arguments = [
                str(argument)
                for argument in self._get_list_safe(run_testcase, "arguments")
            ]
        if "stdin" in run_testcase:
            run.input.stdin = self._get_str_safe(run_testcase, "stdin")

        if "exit_code" in run_testcase:
            run.output.exit_code.value = self._get_int_safe(run_testcase,
                                                            "exit_code")
        self._translate_base_output(run_testcase, run.output, stack_frame)
        return run

    def _translate_stream(self,
                          stream: YAML_OBJECT,
                          config: OPTION_DICT) -> TextOutputChannel:
        if isinstance(stream, (bool, float, int, str)):
            return TextOutputChannel(data=stream)
        stream = self._enforce_dict(stream)
        config = merge(config, self._get_dict_safe(stream, "config"))
        return TextOutputChannel(data=self._get_str_safe(stream, "data"),
                                 evaluator=GenericTextEvaluator(options=config))

    def _translate_tab(self,
                       tab: YAML_DICT,
                       stack_frame: StackFrame = StackFrame()) -> Tab:
        stack_frame = self._translate_config(
            self._get_dict_safe(tab, "config"),
            stack_frame
        )
        hidden = self._get_bool_safe(tab, "hidden")
        name = self._get_str_safe(tab, "tab")
        if "contexts" in tab:
            contexts = [
                self._translate_context(context, stack_frame)
                for context in self._get_list_safe(tab, "contexts")
            ]
            runs = [Run(contexts=contexts)]
        else:
            runs = [
                self._translate_run(run, stack_frame)
                for run in self._get_list_safe(tab, "runs")
            ]
        return Tab(name=name, hidden=hidden, runs=runs)

    def _translate_testcase(self,
                            testcase: YAML_DICT,
                            stack_frame: StackFrame = StackFrame()) -> Testcase:
        testcase_value = Testcase(input=parser.parse_statement(
            self._get_str_safe(testcase, "statement")))
        self._translate_base_output(testcase, testcase_value.output,
                                    stack_frame)
        if "return" in testcase:
            testcase_value.output.result = ValueOutputChannel(
                value=self._translate_value(
                    self._get_any_safe(testcase, "return")))
        elif "return-raw" in testcase:
            testcase_value.output.result = ValueOutputChannel(
                value=parser.parse_value(
                    self._get_str_safe(testcase, "return-raw")))
        return testcase_value

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
        return dumps(json_object, cls=DataclassJSONEncoder, indent=self.indent)

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
