"""
Code related to working with templates.

The vast majority of code in this module is simplified versions of the testplan datastructures, that
are used inside the templates.

By defining the datastructures here explicitly, it allows for better control and checks that we did
not forget some attributes when implementing a language.
"""
import dataclasses
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Union

from mako import exceptions
from mako.template import Template

from serialisation import Value
from testplan import TextData, NoneChannelState, Assignment, FunctionCall


@dataclass
class TestcaseData:
    statement: Union[FunctionCall, Assignment]
    stdin: Union[TextData, NoneChannelState]
    value_code: str
    exception_code: str
    has_return: bool


@dataclass
class MainTestcaseData:
    exists: bool
    exception_code: str
    arguments: List[Value]


@dataclass
class ContextData:
    before: str
    after: str
    secret_id: str
    submission_name: str
    value_file: str
    exception_file: str
    main_testcase: MainTestcaseData
    additional_testcases: List[TestcaseData]


@dataclass
class EvaluatorData:
    main_testcase: MainTestcaseData
    additional_testcases: List[TestcaseData]
    value_file: str
    exception_file: str


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
