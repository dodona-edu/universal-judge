"""Code related to working with templates"""
import dataclasses
from dataclasses import dataclass
from pathlib import Path
from typing import List

from mako.template import Template

from testplan import Input, FunctionCall


@dataclass
class SubmissionData:
    submission: str
    submission_name: str
    before: str
    after: str


@dataclass
class OutputData:
    value_code: str


@dataclass
class TestcaseData:
    input: Input
    output: OutputData


@dataclass
class ContextData:
    submission: str
    before: str
    after: str
    secret_id: str
    context_id: str
    has_top_level: bool
    submission_name: str
    execution: FunctionCall
    output_file: str
    additionals: List[TestcaseData]


def write_template(arguments, template: Template, path: Path):
    fields = dataclasses.fields(arguments)
    values = {field.name: getattr(arguments, field.name) for field in fields}
    result = template.render(**values)
    # noinspection PyTypeChecker
    with open(path, "w") as file:
        file.write(result)
