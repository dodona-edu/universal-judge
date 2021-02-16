"""
Translates items from the testplan into the actual programming language.
"""
import logging
from dataclasses import dataclass
from pathlib import Path
from typing import List, Union, Tuple, Optional, Set, Callable

from mako import exceptions

from .config import TemplateType
from .templates import find_and_write_template, find_template
from ..configs import Bundle
from ..datatypes import BasicSequenceTypes
from ..dodona import ExtendedMessage
from ..serialisation import (Value, SequenceType, Identifier, FunctionType,
                             FunctionCall, Expression, Statement, Assignment,
                             NothingType, NamedArgument)
from ..testplan import (EmptyChannel, IgnoredChannel, TextData, ProgrammedEvaluator,
                        SpecificEvaluator, Testcase, RunTestcase, Context,
                        ExceptionOutput, ValueOutput, Run)
from ..utils import get_args

_logger = logging.getLogger(__name__)

# Names of the predefined functions that must be available.
SEND_VALUE = "send_value"
SEND_EXCEPTION = "send_exception"
SEND_SPECIFIC_VALUE = "send_specific_value"
SEND_SPECIFIC_EXCEPTION = "send_specific_exception"

# Name of the function to call in evaluators
PROGRAMMED_EVALUATE = "evaluate"

EVALUATION_ARGS = "value"

# Names of predefined templates.
STATEMENT = "statement"


@dataclass
class _TestcaseArguments:
    """Arguments for a testcases testcase template."""
    # The input command. In most languages, you can use input_statement instead.
    command: Statement
    _value_function: Optional[Callable[[Expression], Statement]]
    _exception_function: Callable[[Expression], Statement]

    def input_statement(self, override: Optional[str] = None) -> Statement:
        """
        Get the input statement for the testcase.

        This will return, depending on the command, either an expression which will
        pass the value to the correct handling function or a statement.

        :param override: Optionally override the value argument.
        :return: The input statement.
        """
        if self._value_function:
            assert isinstance(self.command, get_args(Expression))
            # Replace the arguments
            if override:
                return self._value_function(Identifier(override))
            else:
                return self._value_function(self.command)
        else:
            return self.command

    def exception_statement(self, name: Optional[str] = None) -> Statement:
        """
        Get the exception statement for the testcase.

        :param name: Optionally the name of an identifier containing the exception.
        :return: The exception statement.
        """
        if name:
            return self._exception_function(Identifier(name))
        else:
            return self._exception_function(NothingType())


@dataclass
class _RunTestcaseArguments:
    """Arguments for a run testcase template."""
    # If a context testcase exists.
    exists: bool
    # Main arguments.
    arguments: List[str]
    _exception_function: Callable[[Expression], Statement]

    def exception_statement(self, name: Optional[str] = None) -> Statement:
        """
        Get the exception statement for the testcase.

        :param name: Optionally the name of an identifier containing the exception.
        :return: The exception statement.
        """
        if name:
            return self._exception_function(Identifier(name))
        else:
            return self._exception_function(NothingType())


@dataclass
class _ContextArguments:
    """Arguments for a plan template for the contexts."""
    # The "before" code.
    before: str
    # The after code.
    after: str
    # A list of the other testcases.
    testcases: List[_TestcaseArguments]


@dataclass
class _ExecutionArguments:
    """Arguments for a plan template for the executions."""
    # The name of the execution.
    execution_name: str
    # The name of the file for the return channel.
    value_file: str
    # The name of the file for the exception channel.
    exception_file: str
    # The name of the submission file.
    submission_name: str
    # The secret ID.
    secret_id: str
    # The secret context ID.
    context_secret_id: str
    # The run testcase
    run_testcase: _RunTestcaseArguments
    # The contexts
    contexts: List[_ContextArguments]
    # A set of the names of the language specific evaluators we will need.
    evaluator_names: Set[str]


@dataclass
class _CustomEvaluatorArguments:
    evaluator: str
    function: FunctionCall


@dataclass
class _SelectorArguments:
    contexts: List[str]


@dataclass
class InternalFunctionCall(FunctionCall):
    has_root_namespace: bool = True
    # TODO: find out why this variable can't be initialized by the constructor


def _prepare_argument(
        bundle: Bundle,
        argument: Union[Expression, NamedArgument]
) -> Union[Expression, NamedArgument]:
    if isinstance(argument, NamedArgument):
        return NamedArgument(name=argument.name,
                             value=_prepare_expression(bundle, argument.value))
    return _prepare_expression(bundle, argument)


def _prepare_expression(bundle: Bundle, expression: Expression) -> Expression:
    """
    Prepare an expression for use in a template.
    """

    if isinstance(expression, FunctionCall):
        if isinstance(expression, InternalFunctionCall):
            expression.arguments = [_prepare_argument(bundle, arg)
                                    for arg in expression.arguments]
            return expression
        else:
            submission_name = bundle.lang_config.submission_name(bundle.plan)
            if expression.type == FunctionType.CONSTRUCTOR:
                name = expression.name
            else:
                name = bundle.lang_config.conventionalize_function(expression.name)

            internal = InternalFunctionCall(
                type=expression.type,
                arguments=[_prepare_argument(bundle, arg)
                           for arg in expression.arguments],
                name=name,
                namespace=expression.namespace or submission_name
            )
            internal.has_root_namespace = not bool(expression.namespace)
            return internal

    return expression


def _create_handling_function(
        bundle: Bundle,
        send_value: str,
        send_evaluated: str,
        output: Union[ExceptionOutput, ValueOutput]
) -> Tuple[Callable[[Expression], Statement], Optional[str]]:
    """
    Create a function to handle the result of a return value or an exception.

    There are two possibilities:
    - There is a language specific evaluator. In that case, we wrap the value in
      a function call to the evaluator, and then send off the result. An example of
      the result:

        send_evaluated(evaluate(value))

    - There is no language specific evaluator. In that case, we just send off the
      value directly. An example of the result:

        send_value(value)

    :param bundle: The configuration bundle.
    :param send_evaluated: The name of the function that will handle sending the
                           result of an evaluation.
    :param send_value: The name of the function that will handle sending the value.
    :param output: The evaluator.
    :return: A tuple containing the call and the name of the evaluator if present.
    """
    lang_config = bundle.lang_config
    if (hasattr(output, "evaluator")
            and isinstance(output.evaluator, SpecificEvaluator)):
        evaluator = output.evaluator.for_language(
            bundle.config.programming_language)
        evaluator_name = lang_config.conventionalize_namespace(evaluator.file.stem)
    else:
        evaluator_name = None

    def generator(expression: Expression) -> Statement:
        if (hasattr(output, "evaluator")
                and isinstance(output.evaluator, SpecificEvaluator)):
            arguments = [InternalFunctionCall(
                type=FunctionType.FUNCTION,
                name=evaluator.name,
                namespace=evaluator_name,
                arguments=[_prepare_expression(bundle, expression)]
            )]
            arguments[0].has_root_namespace = False
            function_name = send_evaluated
        else:
            arguments = [expression]
            function_name = send_value

        internal = InternalFunctionCall(
            type=FunctionType.FUNCTION,
            name=lang_config.conventionalize_function(function_name),
            arguments=[_prepare_argument(bundle, arg) for arg in arguments]
        )
        internal.has_root_namespace = False
        return internal

    return generator, evaluator_name


def _create_exception_function(
        bundle: Bundle,
        testcase: Union[Testcase, RunTestcase]
) -> Tuple[Callable[[Expression], Statement], Optional[str]]:
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
    return _create_handling_function(
        bundle, SEND_EXCEPTION, SEND_SPECIFIC_EXCEPTION, exception_channel
    )


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

    has_return = result_channel not in (EmptyChannel.NONE, IgnoredChannel.IGNORED)

    # Create the function to handle the values.
    value_function_call, evaluator_name = _create_handling_function(
        bundle, SEND_VALUE, SEND_SPECIFIC_VALUE, result_channel
    )
    if evaluator_name:
        names.append(evaluator_name)

    # A special case: if there isn't an actual value, don't call the function.
    if not has_return:
        value_function_call = None
        assert evaluator_name is None

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
        _value_function=value_function_call,
        _exception_function=exception_function_call,
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


def _prepare_run_testcase(
        bundle: Bundle,
        run: Run
) -> Tuple[_RunTestcaseArguments, Optional[str]]:
    """
    Prepare the context testcase for a context.

    :param bundle: The configuration bundle.
    :param context: The context to prepare the context testcase for.

    :return: The testcase arguments and an optional generated file name.
    """
    testcase = run.run
    exception_function, name = _create_exception_function(bundle, testcase)
    if testcase.input.main_call:
        return _RunTestcaseArguments(
            exists=True,
            arguments=testcase.input.arguments,
            _exception_function=exception_function
        ), name
    else:
        return _RunTestcaseArguments(
            exists=False,
            _exception_function=exception_function,
            arguments=[]
        ), name


def get_readable_input(bundle: Bundle,
                       case: Union[Testcase, RunTestcase]) -> ExtendedMessage:
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
        text = bundle.lang_config.cleanup_description(bundle.plan.namespace, text)
    elif isinstance(case, RunTestcase):
        if case.input.main_call:
            arguments = " ".join(case.input.arguments)
            args = f"./submission {arguments}"
            if isinstance(case.input.stdin, TextData):
                stdin = case.input.stdin.get_data_as_string(bundle.config.resources)
            else:
                stdin = ""
            if not stdin:
                text = args
            else:
                if case.input.arguments:
                    text = f"{args}\n{stdin}"
                else:
                    text = stdin
        else:
            text = ""
    else:
        raise AssertionError("Unknown testcase type.")
    return ExtendedMessage(description=text, format=format_)


def attempt_run_readable_input(bundle: Bundle, run: RunTestcase) -> ExtendedMessage:
    result = get_readable_input(bundle, run)
    if result.description:
        return result

    return ExtendedMessage(
        description="Geen invoer gevonden.",
        format="text"
    )


def attempt_readable_input(bundle: Bundle, context: Context) -> ExtendedMessage:
    # Try until we find a testcase with input.
    testcases = context.testcases
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
    template = bundle.lang_config.template_name(TemplateType.STATEMENT)
    if isinstance(statement, get_args(Expression)):
        statement = _prepare_expression(bundle, statement)
        template = find_template(bundle, template)
        try:
            return template.render(statement=statement)
        except Exception as e:
            _logger.error(exceptions.text_error_template().render())
            raise e

    assert isinstance(statement, get_args(Assignment))
    prepared_expression = _prepare_expression(bundle, statement.expression)
    statement = statement.replace_expression(prepared_expression)
    template = find_template(bundle, template)
    try:
        return template.render(statement=statement, full=True)
    except Exception as e:
        _logger.error(exceptions.text_error_template().render())
        raise e


def _generate_context(bundle: Bundle,
                      context: Context) -> Tuple[_ContextArguments, Set[str]]:
    """
    Prepare one context for the execution

    :param bundle: The configuration bundle.
    :param context: The context to prepare

    :return: The prepared context arguments and a set
             of evaluator names.
    """
    language = bundle.config.programming_language
    resources = bundle.config.resources
    before_code = context.before.get(language, TextData(data="")) \
        .get_data_as_string(resources)
    after_code = context.after.get(language, TextData(data="")) \
        .get_data_as_string(resources)
    testcases, evaluator_names = _prepare_testcases(bundle, context)
    return _ContextArguments(
        before=before_code,
        after=after_code,
        testcases=testcases
    ), evaluator_names


def generate_execution(bundle: Bundle,
                       destination: Path,
                       run: Run,
                       execution_name: str) -> Tuple[str, List[str]]:
    """
    Generate the files related to the execution.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param run: The execution for which generation is happening.
    :param execution_name: The name of the execution module.

    :return: The name of the generated file in the given destination and a set
             of evaluator names that will also be needed.
    """
    lang_config = bundle.lang_config
    evaluator_names = set()
    run_testcase, name = _prepare_run_testcase(bundle, run)
    contexts = []
    if name:
        evaluator_names.add(name)
    for context in run.contexts:
        context_args, context_evaluator_names = _generate_context(bundle, context)
        contexts.append(context_args)
        evaluator_names.update(context_evaluator_names)

    value_file_name = value_file(bundle, destination).name
    exception_file_name = exception_file(bundle, destination).name

    submission_name = lang_config.submission_name(bundle.plan)

    execution_args = _ExecutionArguments(
        execution_name=execution_name,
        value_file=value_file_name,
        exception_file=exception_file_name,
        submission_name=submission_name,
        secret_id=bundle.secret,
        context_secret_id=bundle.context_separator_secret,
        run_testcase=run_testcase,
        contexts=contexts,
        evaluator_names=evaluator_names
    )

    evaluator_files = [f"{x}.{lang_config.extension_file()}"
                       for x in evaluator_names]

    execution_destination = destination / lang_config.with_extension(execution_name)
    template = lang_config.template_name(TemplateType.RUN)

    return find_and_write_template(
        bundle, execution_args, execution_destination, template
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
    assert bundle.lang_config.needs_selector()
    selector_name = bundle.lang_config.selector_name()
    destination /= bundle.lang_config.with_extension(selector_name)
    selector = bundle.lang_config.template_name(TemplateType.SELECTOR)
    return find_and_write_template(
        bundle=bundle,
        template_args=_SelectorArguments(contexts=context_names),
        destination=destination,
        template_name=selector
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
    evaluator_name = bundle.lang_config.conventionalize_namespace(
        evaluator.function.file.stem
    )
    arguments = custom_evaluator_arguments(evaluator)

    function = InternalFunctionCall(
        type=FunctionType.FUNCTION,
        namespace=evaluator_name,
        name=evaluator.function.name,
        arguments=[expected_value, actual_value, arguments]
    )
    function.has_root_namespace = False

    args = _CustomEvaluatorArguments(
        evaluator=evaluator_name,
        function=function
    )

    template = bundle.lang_config \
        .template_name(TemplateType.EVALUATOR_EXECUTOR)
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
