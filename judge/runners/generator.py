"""
Translates items from the testplan into the actual programming language.
"""
import dataclasses
from dataclasses import dataclass
from functools import cached_property
from os import PathLike
from pathlib import Path
from typing import List, Union, Any, Tuple, Optional, Set

import sys
from mako import exceptions
from mako.exceptions import TemplateLookupException
from mako.lookup import TemplateLookup
from mako.template import Template

import utils
from dodona import ExtendedMessage
from runners.config import LanguageConfig
from runners.utils import remove_indents, remove_newline
from serialisation import Value, StringType, StringTypes, SequenceType, \
    SequenceTypes
from tested import Config
from testplan import Testcase, NormalTestcase, AssignmentInput, NoneChannelState, \
    TextData, MainTestcase, FunctionInput, FunctionCall, Assignment, FunctionType, \
    Context, NoMainTestcase, IgnoredChannelState, SpecificEvaluator, CustomEvaluator


@dataclass
class TestcaseArguments:
    """Arguments for a normal testcase template."""
    statement: Union[FunctionCall, Assignment]
    stdin: Union[TextData, NoneChannelState]
    has_return: bool
    value_function: FunctionCall
    exception_function: FunctionCall


@dataclass
class MainTestcaseArguments:
    """Arguments for a main testcase template."""
    exists: bool
    exception_function: FunctionCall
    arguments: List[Value]


@dataclass
class ContextArguments:
    """Arguments for a plan template for the contexts."""
    context_name: str
    before: str
    after: str
    main_testcase: MainTestcaseArguments
    additional_testcases: List[TestcaseArguments]
    evaluator_names: Set[str]
    value_file: str
    exception_file: str
    submission_name: str
    value_file: str
    exception_file: str
    secret_id: str


@dataclass
class CustomEvaluatorArguments:
    evaluator: str
    expected: Value
    actual: Value
    arguments: Value


@dataclass
class SelectorArguments:
    contexts: List[str]


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


def path_to_templates(
        language_config: LanguageConfig,
        judge_config: Config,
) -> List[Path]:
    """
    Construct the path to the templates folder.
    :param judge_config: The judge config.
    :param language_config: The language configuration.
    :return:
    """
    judge_directory = Path(judge_config.judge)
    result = []
    for end in language_config.template_folders(judge_config.programming_language):
        result.append(judge_directory / 'judge' / 'runners' / 'templates' / end)
    return result


def value_file(working_directory: Path, secret: str):
    """
    Get the path to a resulting value file.
    :param working_directory: The execution directory.
    :param secret: The secret for this execution.
    :return: The path to the file, depending on the working directory.
    """
    return working_directory / f"{secret}_values.txt"


def exception_file(working_directory: Path, secret: str):
    """
    Get the path to a resulting exception file.
    :param working_directory: The execution directory.
    :param secret: The secret for this execution.
    :return: The path to the file, depending on the working directory.
    """
    return working_directory / f"{secret}_exceptions.txt"


def prepare_function_call(
        config: LanguageConfig,
        submission_name: str,
        function_call: FunctionCall
) -> FunctionCall:
    """
    Prepare the function call for use. This will conventionalise the function name
    for the used language and inject the submission name for top-level functions.
    """
    if function_call.type == FunctionType.IDENTITY:
        return function_call
    return FunctionCall(
        type=function_call.type,
        arguments=function_call.arguments,
        name=config.conventionalise(function_call.name),
        object=function_call.object or submission_name
    )


def get_exception_function(
        language_config: LanguageConfig,
        programming_language: str,
        testcase: Union[Testcase, NoMainTestcase]
) -> Tuple[FunctionCall, Optional[str]]:
    """
    Extract a function call for handling an exception of a testcase.
    :param language_config: The config for the language.
    :param programming_language: The language we want.
    :param testcase: The testcase to extract the function from.
    :return: The extracted function.
    """
    if isinstance(testcase, Testcase):
        exceptions_matters = not isinstance(
            testcase.output.exception, (IgnoredChannelState, NoneChannelState)
        )
        has_language_specific_evaluator_exception = (
                exceptions_matters and
                isinstance(testcase.output.exception.evaluator, SpecificEvaluator)
        )
        # If a language specific evaluator is needed, generate the code to call it.
        if has_language_specific_evaluator_exception:
            evaluator = testcase.output.exception.evaluator.evaluators[
                programming_language
            ]
            # We can assume the class is already imported.
            evaluator_name = language_config.conventionalise_object(
                utils.basename(evaluator)
            )
            return FunctionCall(
                type=FunctionType.NAMESPACE,
                name="evaluate",
                object=evaluator_name,
                arguments=[StringType(type=StringTypes.LITERAL, data="value")]
            ), evaluator_name

    # In all other cases, we return the default function, which sends the
    # exception to the judge for further processing.
    return FunctionCall(
        type=FunctionType.FUNCTION,
        name=language_config.conventionalise("send_exception"),
        arguments=[StringType(type=StringTypes.LITERAL, data="value")]
    ), None


def prepare_testcase(
        language_config: LanguageConfig,
        programming_language: str,
        submission_name: str,
        testcase: Testcase
) -> Tuple[TestcaseArguments, List[str]]:
    """
    Prepare a testcase. This will prepare any function calls or assignments, and
    extract functions for handling return values and exceptions.
    :param language_config: The language config.
    :param programming_language: The programming language we want.
    :param submission_name: The name of the submission.
    :param testcase: The testcase to prepare.
    :return: Arguments containing the preparation results and the evaluator name or
             None if no language specific evaluator is needed.
    """
    names = []
    # Prepare the evaluation code.
    output_matters = not isinstance(
        testcase.output.result, (IgnoredChannelState, NoneChannelState)
    )
    has_language_specific_evaluator_value = (
            output_matters and
            isinstance(testcase.output.result.evaluator, SpecificEvaluator)
    )

    # If a language specific evaluator is needed, generate the code to call it.
    if has_language_specific_evaluator_value:
        evaluator = testcase.output.result.evaluator.evaluators[
            programming_language
        ]
        # We can assume the class is already imported.
        evaluator_name = language_config.conventionalise_object(
            utils.basename(evaluator)
        )
        call = FunctionCall(
            type=FunctionType.NAMESPACE,
            name=language_config.conventionalise("evaluate"),
            object=language_config.conventionalise(evaluator_name),
            arguments=[StringType(type=StringTypes.LITERAL, data="value")]
        )
        value_function_call = language_config.specific_evaluator_callback(call)
        names.append(evaluator_name)
    else:
        value_function_call = FunctionCall(
            type=FunctionType.FUNCTION,
            name="send",
            arguments=[StringType(type=StringTypes.LITERAL, data="value")]
        )

    exception_function_call, exception_evaluator_name \
        = get_exception_function(language_config, programming_language, testcase)
    if exception_evaluator_name:
        names.append(exception_evaluator_name)

    # Convert the function call.
    has_return = (testcase.output.result != NoneChannelState.NONE and
                  isinstance(testcase.input, FunctionInput))
    if isinstance(testcase.input, FunctionInput):
        statement = prepare_function_call(
            language_config,
            submission_name,
            testcase.input.function
        )
    else:
        assert isinstance(testcase.input, AssignmentInput)
        statement = testcase.input.assignment.replace_function(
            prepare_function_call(
                language_config,
                submission_name,
                testcase.input.assignment.expression
            ))

    return TestcaseArguments(
        statement=statement,
        stdin=testcase.input.stdin,
        has_return=has_return,
        value_function=value_function_call,
        exception_function=exception_function_call
    ), names


class Generator:
    """
    Used to translate constructs of the testplan into the actual programming
    language. By default, templates are used.
    """

    def __init__(self,
                 judge_config: Config,
                 language_config: LanguageConfig):
        self.language_config = language_config
        self.judge_config = judge_config

    def get_readable_input(self,
                           submission_name: str,
                           case: Testcase) -> ExtendedMessage:
        """
        Get human readable input for a testcase. This function will use, in
        order of availability:

        1. A description on the testcase.
        2. A function call or assignment.
        3. The stdin.
        4. Program arguments, if any.

        :param case: The testcase to get the input from.
        :param submission_name: The name of the submission.
        """
        format_ = 'text'  # By default, we use text as input.
        if case.description:
            text = case.description
        elif isinstance(case, NormalTestcase) \
                and isinstance(case.input, FunctionInput):
            text = self.function_call(submission_name, case.input.function)
            format_ = self.judge_config.programming_language
        elif isinstance(case, NormalTestcase) \
                and isinstance(case.input, AssignmentInput):
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

    @cached_property
    def _get_environment(self) -> TemplateLookup:
        """Get the environment for the templates."""
        preprocessors = [remove_indents, remove_newline]
        paths = [str(x) for x in
                 path_to_templates(self.language_config, self.judge_config)]
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

    def function_call(self, submission_name: str, call: FunctionCall) -> str:
        """Translate a function to code."""
        call = prepare_function_call(self.language_config, submission_name, call)
        template = self.find_template("function")
        return template.render(function=call)

    def assignment(self, assignment: Assignment) -> str:
        """Translate an assignment to code."""
        assignment = assignment.replace_function(assignment.expression)
        template = self.find_template("assignment")
        return template.render(assignment=assignment, full=True)

    def custom_evaluator(self, args: CustomEvaluatorArguments,
                         destination: Path) -> str:
        return self._find_and_write_template(args, destination,
                                             "evaluator_executor")

    def _find_and_write_template(self, args: Any,
                                 destination: Union[PathLike, Path],
                                 name: str, result: Optional[str] = None) -> str:
        name = self.language_config.conventionalise_object(name)
        if not result:
            result = name
        template = self.find_template(name)
        destination_name = f"{result}.{self.language_config.file_extension()}"
        write_template(args, template, destination / destination_name)
        return destination_name

    def generate_context(self,
                         secret: str,
                         destination: Path,
                         context: Context,
                         context_name: str,
                         submission_name: str) -> Tuple[str, List[str]]:
        """
        Generate the files related to the context.
        :param secret: The secret for the execution.
        :param destination: Where the generated files should go.
        :param context: The context for which generation is happening.
        :param context_name: The name of the context module.
        :param submission_name: The name of the submission from the user.
        :return: The name of the generated file in the given destination and a list
                 of evaluator files that will also be needed.
        """
        before_code = context.before.get(self.judge_config.programming_language, "")
        after_code = context.after.get(self.judge_config.programming_language, "")

        value_file_name = value_file(destination, secret).name
        exception_file_name = exception_file(destination, secret).name

        additional_testcases, evaluator_names = self.prepare_normal_testcases(
            submission_name,
            context
        )
        main_testcase, main_name = self.prepare_main_testcases(context)
        if main_name:
            evaluator_names.add(main_name)

        context_argument = ContextArguments(
            context_name=context_name,
            before=before_code,
            after=after_code,
            main_testcase=main_testcase,
            additional_testcases=additional_testcases,
            value_file=value_file_name,
            exception_file=exception_file_name,
            submission_name=submission_name,
            secret_id=secret,
            evaluator_names=evaluator_names
        )

        evaluator_files = [f"{x}.{self.language_config.file_extension()}"
                           for x in evaluator_names]

        return self._find_and_write_template(
            args=context_argument,
            destination=destination,
            name="context",
            result=context_name
        ), evaluator_files

    def prepare_normal_testcases(
            self,
            submission_name: str,
            context: Context
    ) -> Tuple[List[TestcaseArguments], Set[str]]:
        """
        Get a list of all normal testcases in a context.
        :param submission_name:
        :param context:
        :return:
        """
        result = []
        names = set()
        for i, testcase in enumerate(context.normal):
            args, new_names = prepare_testcase(
                language_config=self.language_config,
                programming_language=self.judge_config.programming_language,
                submission_name=submission_name,
                testcase=testcase
            )
            result.append(args)
            names.update(new_names)
        return result, names

    def prepare_main_testcases(
            self,
            context: Context
    ) -> Tuple[MainTestcaseArguments, Optional[str]]:
        """Prepare the main testcase for use."""
        exception_function, name = get_exception_function(
            language_config=self.language_config,
            programming_language=self.judge_config.programming_language,
            testcase=context.main
        )
        if context.main == NoMainTestcase.NONE:
            return MainTestcaseArguments(
                exists=False,
                exception_function=exception_function,
                arguments=[]
            ), name
        else:
            return MainTestcaseArguments(
                exists=True,
                arguments=context.main.input.arguments,
                exception_function=exception_function
            ), name

    def generate_selector(self,
                          destination: Path,
                          context_names: List[str]) -> str:
        """
        Generate the file to execute_module a single context.
        :param destination: Where the generated files should go.
        :param context_names: The names of the contexts.
        :return: The name of the generated file in the given destination.
        """
        assert self.language_config.needs_selector()
        return self._find_and_write_template(
            args=SelectorArguments(contexts=context_names),
            destination=destination,
            name="selector"
        )

    def generate_custom_evaluator(self,
                                  destination: Path,
                                  evaluator: CustomEvaluator,
                                  actual: Value,
                                  expected: Value) -> str:
        """
        Generate the code for the custom evaluator.
        :param destination: The folder to generate in.
        :param evaluator: The evaluator data.
        :param actual: The actual data, received from the code.
        :param expected: The expected data, from the testplan.
        :return: The generated file.
        """
        evaluator_name = self.language_config.conventionalise_object(
            evaluator.path.stem
        )
        arguments = SequenceType(
            type=SequenceTypes.SEQUENCE,
            data=evaluator.arguments
        )

        args = CustomEvaluatorArguments(
            evaluator=evaluator_name,
            expected=expected,
            actual=actual,
            arguments=arguments
        )

        return self.custom_evaluator(args, destination)
