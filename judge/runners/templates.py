"""Code related to working with templates"""
import dataclasses
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Union

from mako import exceptions
from mako.template import Template

from serialisation import Value
from testplan import FunctionCall, TextData, NoneChannelState


@dataclass
class SubmissionData:
    submission: str
    submission_name: str
    before: str
    after: str


@dataclass
class TestcaseData:
    function: FunctionCall
    stdin: Union[TextData, NoneChannelState]
    value_code: str
    exception_code: str
    has_return: bool


@dataclass
class ExecutionTestcaseData:
    exists: bool
    exception_code: str
    arguments: List[Value]


@dataclass
class ContextData:
    submission: str
    before: str
    after: str
    secret_id: str
    context_id: str
    submission_name: str
    execution: ExecutionTestcaseData
    value_file: str
    exception_file: str
    additionals: List[TestcaseData]


@dataclass
class EvaluatorData:
    execution: ExecutionTestcaseData
    additionals: List[TestcaseData]
    value_file: str
    exception_file: str
    context_id: str


@dataclass
class CustomData:
    evaluator_code: str
    expected: Value
    actual: Value


def write_template(arguments, template: Template, path: Path):
    fields = dataclasses.fields(arguments)
    values = {field.name: getattr(arguments, field.name) for field in fields}
    try:
        result = template.render(**values)
    except Exception as e:
        print(exceptions.text_error_template().render(), file=sys.stderr)
        raise e
    # noinspection PyTypeChecker
    with open(path, "w") as file:
        file.write(result)
