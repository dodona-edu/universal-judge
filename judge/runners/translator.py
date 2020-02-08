"""
Translates items from the testplan into the actual programming language.
"""
import dataclasses
from dataclasses import dataclass
from os import PathLike

import sys
from functools import cached_property
from mako import exceptions
from mako.exceptions import TemplateLookupException
from mako.lookup import TemplateLookup
from mako.template import Template
from pathlib import Path
from typing import List, Union, Any

from dodona import ExtendedMessage
from runners.config import LanguageConfig
from runners.utils import remove_indents, remove_newline
from serialisation import Value
from tested import Config
from testplan import Testcase, NormalTestcase, AssignmentInput, NoneChannelState, TextData, MainTestcase, \
    FunctionInput, FunctionCall, Assignment, FunctionType, Plan


@dataclass
class TestcaseArguments:
    """Arguments for a normal testcase template."""
    statement: Union[FunctionCall, Assignment]
    stdin: Union[TextData, NoneChannelState]
    value_code: str
    exception_code: str
    has_return: bool


@dataclass
class MainTestcaseArguments:
    """Arguments for a main testcase template."""
    exists: bool
    exception_code: str
    arguments: List[Value]


@dataclass
class EvaluatorArguments:
    main_testcase: MainTestcaseArguments
    additional_testcases: List[TestcaseArguments]


@dataclass
class ContextArguments(EvaluatorArguments):
    """Arguments for a context template."""
    before: str
    after: str


@dataclass
class PlanContextArguments:
    """Arguments for a plan template for the contexts."""
    secret_id: str
    contexts: List[ContextArguments]
    value_file: str
    exception_file: str
    submission_name: str


@dataclass
class PlanEvaluatorArguments:
    """Arguments for a plan template for the evaluators"""
    value_file: str
    exception_file: str
    secret_id: str
    evaluators: List[EvaluatorArguments]


@dataclass
class CustomEvaluatorArguments:
    evaluator: str
    expected: Value
    actual: Value
    arguments: Value


def write_template(arguments, template: Template, path: PathLike):
    """
    Write a template with the arguments as a data class.
    :param arguments: The arguments for the template. Should be a dataclass.
    :param template: The template to write.
    :param path: Where to write the template to.
    """
    fields = dataclasses.fields(arguments)
    values = {field.name: getattr(arguments, field.name) for field in fields}
    try:
        result = template.render(**values)
    except Exception as e:
        print(exceptions.text_error_template().render(), file=sys.stderr)
        raise e
    with open(path, "w") as file:
        file.write(result)


class Translator:
    """
    Used to translate constructs of the testplan into the actual programming
    language. By default, templates are used.
    """

    def __init__(self,
                 judge_config: Config,
                 language_config: LanguageConfig):
        self.language_config = language_config
        self.judge_config = judge_config

    def get_readable_input(self, plan: Plan, case: Testcase) -> ExtendedMessage:
        """
        Get human readable input for a testcase. This function will use, in
        order of availability:

        1. A description on the testcase.
        2. A function call or assignment.
        3. The stdin.
        4. Program arguments, if any.

        :param case: The testcase to get the input from.
        :param plan: The plan.
        """
        format_ = 'text'  # By default, we use text as input.
        if case.description:
            text = case.description
        elif isinstance(case, NormalTestcase) and isinstance(case.input, FunctionInput):
            text = self.function_call(plan, case.input.function)
            format_ = self.judge_config.programming_language
        elif isinstance(case, NormalTestcase) and isinstance(case.input, AssignmentInput):
            text = self.assignment(case.input.assignment)
            format_ = self.judge_config.programming_language
        elif case.input.stdin != NoneChannelState.NONE:
            assert isinstance(case.input.stdin, TextData)
            text = case.input.stdin.get_data_as_string(self.judge_config.resources)
        else:
            assert isinstance(case, MainTestcase)
            if case.input.arguments:
                variable_part = str(case.input.arguments)
            else:
                variable_part = "without arguments"
            text = f"Main call: {variable_part}"
        return ExtendedMessage(description=text, format=format_)

    def path_to_templates(self) -> List[Path]:
        """The path to the templates and normal files."""
        result = []
        for end in self.language_config.template_folders(self.judge_config.programming_language):
            result.append(Path(self.judge_config.judge) / 'judge' / 'runners' / 'templates' / end)
        return result

    @cached_property
    def _get_environment(self) -> TemplateLookup:
        """Get the environment for the templates."""
        preprocessors = [remove_indents, remove_newline]
        paths = [str(x) for x in self.path_to_templates()]
        return TemplateLookup(directories=paths, preprocessor=preprocessors)

    def find_template(self, name: str) -> Template:
        """
        Find a template with a name. The function will attempt to find a
        template with the given name and any of the allowed extensions for this
        language. If nothing is found, an error is thrown.
        """
        last_error = None
        for extension in self.language_config.template_extensions():
            try:
                return self._get_environment.get_template(f"{name}.{extension}")
            except TemplateLookupException as e:
                last_error = e
        raise last_error

    def prepare_function_call(self, plan: Plan, function_call: FunctionCall) -> FunctionCall:
        """Prepare the function call for main."""
        if function_call.type == FunctionType.IDENTITY:
            return function_call
        object_ = function_call.object or self.language_config.submission_name(plan)
        return FunctionCall(
            type=function_call.type,
            arguments=function_call.arguments,
            name=self.language_config.conventionalise(function_call.name),
            object=object_
        )

    def function_call(self, plan: Plan, call: FunctionCall) -> str:
        """Translate a function to code."""
        call = self.prepare_function_call(plan, call)
        template = self.find_template("function")
        return template.render(function=call)

    def assignment(self, assignment: Assignment) -> str:
        """Translate an assignment to code."""
        assignment = assignment.replace_function(assignment.expression)
        template = self.find_template("assignment")
        return template.render(assignment=assignment, full=True)

    def custom_evaluator(self, args: CustomEvaluatorArguments, destination: Path) -> str:
        return self._find_and_write_template(args, destination, "evaluator_executor")

    def _find_and_write_template(self, args: Any, destination: Union[PathLike, Path], name: str) -> str:
        name = self.language_config.conventionalise_object(name)
        template = self.find_template(name)
        destination_name = f"{name}.{self.language_config.file_extension()}"
        write_template(args, template, destination / destination_name)
        return destination_name

    def write_plan_context_template(self, args: PlanContextArguments, destination: Path) -> str:
        return self._find_and_write_template(args, destination, "contexts")

    def write_plan_evaluator_template(self, args: PlanEvaluatorArguments, destination: Path) -> str:
        return self._find_and_write_template(args, destination, "evaluators")
