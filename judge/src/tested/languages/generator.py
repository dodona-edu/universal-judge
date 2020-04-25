"""
Translates items from the testplan into the actual programming language.
"""
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import List, Union, Tuple, Optional, Set

from .config import TemplateType
from .templates import find_and_write_template, find_template
from ..configs import Bundle
from ..datatypes import BasicSequenceTypes
from ..dodona import ExtendedMessage
from ..serialisation import (Value, SequenceType, Identifier, FunctionType,
                             FunctionCall, Expression, Statement, Assignment)
from ..testplan import (EmptyChannel, IgnoredChannel, TextData, ProgrammedEvaluator,
                        SpecificEvaluator, Testcase, ContextTestcase, Context)
from ..utils import get_args

_logger = logging.getLogger(__name__)

# Names of the predefined functions that must be available.
SEND_VALUE = "send"
SEND_EXCEPTION = "send_exception"
SEND_SPECIFIC_VALUE = "send_specific_value"
SEND_SPECIFIC_EXCEPTION = "send_specific_exception"

# Name of the function to call in evaluators
EVALUATE = "evaluate"

# Names of predefined templates.
STATEMENT = "statement"


@dataclass
class _TestcaseArguments:
    """Arguments for a testcases testcase template."""
    command: Statement
    value_function: Optional[FunctionCall]
    exception_function: FunctionCall


@dataclass
class _ContextTestcaseArguments:
    """Arguments for a context_testcase testcase template."""
    exists: bool
    exception_function: FunctionCall
    arguments: List[str]


@dataclass
class _ContextArguments:
    """Arguments for a plan template for the contexts."""
    context_name: str
    before: str
    after: str
    context_testcase: _ContextTestcaseArguments
    testcases: List[_TestcaseArguments]
    evaluator_names: Set[str]
    value_file: str
    exception_file: str
    submission_name: str
    value_file: str
    exception_file: str
    secret_id: str


@dataclass
class _CustomEvaluatorArguments:
    evaluator: str
    expected: Value
    actual: Value
    arguments: Value


@dataclass
class _SelectorArguments:
    contexts: List[str]


def _prepare_expression(bundle: Bundle, expression: Expression) -> Expression:
    """
    Prepare an expression for use in a template.
    """
    if isinstance(expression, FunctionCall):
        submission_name = bundle.lang_config.c_submission_name(bundle.plan)
        return FunctionCall(
            type=expression.type,
            arguments=expression.arguments,
            name=bundle.lang_config.c_conventionalize_function(expression.name),
            namespace=expression.namespace or submission_name
        )

    return expression


def _create_exception_function(
        bundle: Bundle,
        testcase: Union[Testcase, ContextTestcase]
) -> Tuple[FunctionCall, Optional[str]]:
    """
    Create a function call for handling exceptions. These functions assume there is
    a variable called "value", which must be reachable from where the function will
    be called.

    :param bundle: The configuration bundle.
    :param testcase: The testcase to create the function for.

    :return: The function and optionally the name of the evaluator file.
    """
    # If we have a regular testcase, handle special evaluators.

    exception_channel = testcase.output.exception
    language = bundle.config.programming_language
    lang_config = bundle.lang_config
    arguments = [Identifier("value")]

    # If exceptions are checked and we have a language specific evaluator, generate
    # the code to call that evaluator.
    if (not isinstance(exception_channel, (IgnoredChannel, EmptyChannel))
            and isinstance(exception_channel.evaluator, SpecificEvaluator)):
        evaluator = exception_channel.evaluator.for_language(language)
        evaluator_name = lang_config.c_conventionalize_namespace(evaluator.stem)

        return FunctionCall(
            type=FunctionType.FUNCTION,
            name=lang_config.c_conventionalize_function(SEND_SPECIFIC_EXCEPTION),
            arguments=[FunctionCall(
                type=FunctionType.NAMESPACE,
                name=lang_config.c_conventionalize_function(EVALUATE),
                namespace=evaluator_name,
                arguments=arguments
            )]
        ), evaluator_name

    # In all other cases, we return the default function, which sends the
    # exception to the judge for further processing.
    return FunctionCall(
        type=FunctionType.FUNCTION,
        name=lang_config.c_conventionalize_function(SEND_EXCEPTION),
        arguments=arguments
    ), None


def _prepare_testcase(
        bundle: Bundle,
        testcase: Testcase) -> Tuple[_TestcaseArguments, List[str]]:
    """
    Prepare a testcase. This will prepare any function calls or assignments, and
    extract functions for handling return values and exceptions.

    :param bundle: The configuration bundle.
    :param testcase: The testcase to prepare.

    :return: Arguments containing the preparation results and the evaluator name or
             None if no language specific evaluator is needed.
    """
    names = []

    result_channel = testcase.output.result
    language = bundle.config.programming_language
    lang_config = bundle.lang_config

    has_return = result_channel not in (EmptyChannel.NONE, IgnoredChannel.IGNORED)

    # Handle the return values.
    if has_return:
        # Generate the code to call language specific evaluators.
        if (hasattr(result_channel, "evaluator")
                and isinstance(result_channel.evaluator, SpecificEvaluator)):
            # Call the the evaluator itself does not write the result out to the
            # correct file, so wrap it in another call.
            # This basically generates a function like this:
            # send_specific_value(evaluator(value))
            evaluator = result_channel.evaluator.for_language(language)
            evaluator_name = lang_config.c_conventionalize_namespace(evaluator.stem)
            value_function_call = FunctionCall(
                type=FunctionType.FUNCTION,
                name=lang_config.c_conventionalize_function(SEND_SPECIFIC_VALUE),
                arguments=[FunctionCall(
                    type=FunctionType.NAMESPACE,
                    name=lang_config.c_conventionalize_function(EVALUATE),
                    namespace=evaluator_name,
                    arguments=[Identifier("value")]
                )]
            )
            names.append(evaluator_name)
        else:
            value_function_call = FunctionCall(
                type=FunctionType.FUNCTION,
                name="send",
                arguments=[Identifier("value")]
            )
    else:
        value_function_call = None

    (exception_function_call,
     exception_evaluator_name) = _create_exception_function(bundle, testcase)

    if exception_evaluator_name:
        names.append(exception_evaluator_name)

    if isinstance(testcase.input, get_args(Expression)):
        command = _prepare_expression(bundle, testcase.input)
    else:
        assert isinstance(testcase.input, get_args(Assignment))
        prepared = _prepare_expression(bundle, testcase.input.expression)
        command = testcase.input.replace_expression(prepared)

    return _TestcaseArguments(
        command=command,
        value_function=value_function_call,
        exception_function=exception_function_call,
    ), names


def _prepare_testcases(
        bundle: Bundle,
        context: Context) -> Tuple[List[_TestcaseArguments], Set[str]]:
    """
    Prepare all testcase in a context.

    :param bundle: The configuration bundle.
    :param context: The context to prepare the testcases for.

    :return: The testcase arguments and a set of generated file names.
    """
    result = []
    files = set()
    for i, testcase in enumerate(context.testcases):
        args, new_names = _prepare_testcase(bundle, testcase)
        result.append(args)
        files.update(new_names)
    return result, files


def _prepare_context_testcase(
        bundle: Bundle,
        context: Context
) -> Tuple[_ContextTestcaseArguments, Optional[str]]:
    """
    Prepare the context testcase for a context.

    :param bundle: The configuration bundle.
    :param context: The context to prepare the context testcase for.

    :return: The testcase arguments and an optional generated file name.
    """
    testcase = context.context_testcase
    exception_function, name = _create_exception_function(bundle, testcase)
    if testcase.input.main_call:
        return _ContextTestcaseArguments(
            exists=True,
            arguments=testcase.input.arguments,
            exception_function=exception_function
        ), name
    else:
        return _ContextTestcaseArguments(
            exists=False,
            exception_function=exception_function,
            arguments=[]
        ), name


def get_readable_input(bundle: Bundle,
                       case: Union[Testcase, ContextTestcase]) -> ExtendedMessage:
    """
    Get human readable input for a testcase. This function will use, in
    order of availability:

    1. A description on the testcase.
    2. If it is a normal testcase:
        a. A function expression or convert_statement.
    3. If it is a context testcase:
        a. The stdin and the arguments.
    """
    format_ = 'text'  # By default, we use text as input.
    if case.description:
        text = case.description
    elif isinstance(case, Testcase):
        format_ = bundle.config.programming_language
        text = convert_statement(bundle, case.input)
    elif isinstance(case, ContextTestcase):
        args = f"Argumenten: {case.input.arguments}"
        if isinstance(case.input.stdin, TextData):
            stdin = case.input.stdin.get_data_as_string(bundle.config.resources)
        else:
            stdin = ""
        if case.input.arguments:
            text = f"{args}\n{stdin}"
        else:
            text = stdin
    else:
        raise AssertionError("Unknown testcase type.")
    return ExtendedMessage(description=text, format=format_)


def attempt_readable_input(bundle: Bundle, context: Context) -> ExtendedMessage:
    # Try until we find a testcase with input.
    testcases = [context.context_testcase, *context.testcases]
    for testcase in testcases:
        result = get_readable_input(bundle, testcase)
        if result.description:
            return result

    return ExtendedMessage(
        description="Geen invoer gevonden.",
        format="text"
    )


def convert_statement(bundle: Bundle, statement: Statement) -> str:
    """
    Convert a statement to actual code for the given programming language.
    This will use the "full" mode, meaning type annotation will be present. For
    example, in "default" mode, Java code will look like this:

        test = namespace.functionCall("Hi");

    In full mode, this will be:

        String test = namespace.functionCall("Hi");

    :param bundle: The configuration bundle.
    :param statement: The statement to convert.

    :return: The code the statement.
    """
    template = bundle.lang_config.c_template_name(TemplateType.STATEMENT)
    if isinstance(statement, get_args(Expression)):
        statement = _prepare_expression(bundle, statement)
        template = find_template(bundle, template)
        return template.render(statement=statement)

    assert isinstance(statement, get_args(Assignment))
    prepared_expression = _prepare_expression(bundle, statement.expression)
    statement = statement.replace_expression(prepared_expression)
    template = find_template(bundle, template)
    return template.render(statement=statement, full=True)


def generate_context(bundle: Bundle,
                     destination: Path,
                     context: Context,
                     context_name: str) -> Tuple[str, List[str]]:
    """
    Generate the files related to the context.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param context: The context for which generation is happening.
    :param context_name: The name of the context module.

    :return: The name of the generated file in the given destination and a list
             of evaluator files that will also be needed.
    """
    language = bundle.config.programming_language
    lang_config = bundle.lang_config
    before_code = context.before.get(language, "")
    after_code = context.after.get(language, "")

    value_file_name = value_file(bundle, destination).name
    exception_file_name = exception_file(bundle, destination).name

    testcases, evaluator_names = _prepare_testcases(bundle, context)
    context_testcase, context_eval_name = _prepare_context_testcase(bundle, context)
    if context_eval_name:
        evaluator_names.add(context_eval_name)

    submission_name = lang_config.c_submission_name(bundle.plan)

    context_argument = _ContextArguments(
        context_name=context_name,
        before=before_code,
        after=after_code,
        context_testcase=context_testcase,
        testcases=testcases,
        value_file=value_file_name,
        exception_file=exception_file_name,
        submission_name=submission_name,
        secret_id=bundle.secret,
        evaluator_names=evaluator_names
    )

    evaluator_files = [f"{x}.{lang_config.p_extension_file()}"
                       for x in evaluator_names]

    context_destination = destination / lang_config.with_extension(context_name)
    template = lang_config.c_template_name(TemplateType.CONTEXT)

    return find_and_write_template(
        bundle, context_argument, context_destination, template
    ), evaluator_files


def generate_selector(bundle: Bundle,
                      destination: Path,
                      context_names: List[str]) -> str:
    """
    Generate the file to execute_module a single context.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param context_names: The names of the contexts.

    :return: The name of the generated file in the given destination.
    """
    assert bundle.lang_config.p_needs_selector()
    selector_name = bundle.lang_config.c_selector_name()
    destination /= bundle.lang_config.with_extension(selector_name)
    return find_and_write_template(
        bundle=bundle,
        template_args=_SelectorArguments(contexts=context_names),
        destination=destination,
        template_name=bundle.lang_config.c_template_name(TemplateType.SELECTOR)
    )


def custom_evaluator_arguments(evaluator: ProgrammedEvaluator) -> Value:
    return SequenceType(
        type=BasicSequenceTypes.SEQUENCE,
        data=evaluator.arguments
    )


def generate_custom_evaluator(bundle: Bundle,
                              destination: Path,
                              evaluator: ProgrammedEvaluator,
                              expected_value: Value,
                              actual_value: Value) -> str:
    """
    Generate the code for running a programmed evaluator.

    :param bundle: The configuration bundle.
    :param destination: The folder where the code should be generated.
    :param evaluator: The evaluator data from the testplan.
    :param expected_value: The preprocessed expected value.
    :param actual_value: The preprocessed actual value.

    :return: The name of the generated file.
    """
    evaluator_name = bundle.lang_config.c_conventionalize_namespace(
        evaluator.path.stem
    )
    arguments = custom_evaluator_arguments(evaluator)

    args = _CustomEvaluatorArguments(
        evaluator=evaluator_name,
        expected=expected_value,
        actual=actual_value,
        arguments=arguments
    )

    template = bundle.lang_config \
        .c_template_name(TemplateType.EVALUATOR_EXECUTOR)
    return find_and_write_template(bundle, args, destination, template)


def value_file(bundle: Bundle, directory: Path):
    """
    Return the path to the value file. The file will be placed inside the given
    working directory.

    :param bundle: The configuration bundle.
    :param directory: The directory in which to place the file.

    :return: The path to the file, depending on the working directory.
    """
    return directory / f"{bundle.secret}_values.txt"


def exception_file(bundle: Bundle, directory: Path):
    """
    Return the path to the exception file. The file will be placed inside the given
    working directory.

    :param bundle: The configuration bundle.
    :param directory: The directory in which to place the file.

    :return: The path to the file, depending on the working directory.
    """
    return directory / f"{bundle.secret}_exceptions.txt"
