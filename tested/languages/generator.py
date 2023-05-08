"""
Translates items from the test suite into the actual programming language.
"""
import dataclasses
import html
import json
import logging
import re
import shlex
from dataclasses import dataclass
from pathlib import Path
from typing import (
    TYPE_CHECKING,
    Callable,
    Iterable,
    List,
    Match,
    Optional,
    Set,
    Tuple,
    Union,
)

from pygments.formatters.html import HtmlFormatter

# Prevent cyclic imports for types...
if TYPE_CHECKING:
    from tested.judge.execution import ExecutionUnit

from tested.configs import Bundle
from tested.datatypes import BasicSequenceTypes
from tested.dodona import ExtendedMessage
from tested.internationalization import get_i18n_string
from tested.languages.description_generator import highlight_console
from tested.serialisation import (
    Assignment,
    Expression,
    FunctionCall,
    FunctionType,
    Identifier,
    NamedArgument,
    NothingType,
    ObjectKeyValuePair,
    ObjectType,
    SequenceType,
    Statement,
    Value,
    VariableType,
)
from tested.testsuite import (
    Context,
    EmptyChannel,
    ExceptionOutput,
    FileUrl,
    IgnoredChannel,
    MainInput,
    ProgrammedEvaluator,
    SpecificEvaluator,
    Testcase,
    TextData,
    ValueOutput,
)
from tested.utils import get_args

_logger = logging.getLogger(__name__)
_html_formatter = HtmlFormatter()

# Names of the predefined functions that must be available.
SEND_VALUE = "send_value"
SEND_EXCEPTION = "send_exception"
SEND_SPECIFIC_VALUE = "send_specific_value"
SEND_SPECIFIC_EXCEPTION = "send_specific_exception"


@dataclass
class PreparedTestcaseStatement:
    """
    A testcase that has been prepared for code generation.


    """

    statement: Statement
    "The original statement."
    value_function: Optional[Callable[[Expression], Statement]]
    "The function to handle the return value of the statement, if any."

    def input_statement(self, override: Optional[str] = None) -> Statement:
        """
        Get the input statement for the testcase.

        This will return, depending on the command, either an expression which will
        pass the value to the correct handling function or the statement itself.

        :param override: Optionally override the value argument.
        :return: The input statement.
        """
        if self.value_function:
            assert isinstance(self.statement, get_args(Expression))
            if override:
                return self.value_function(Identifier(override))
            else:
                # noinspection PyTypeChecker
                return self.value_function(self.statement)
        else:
            return self.statement


@dataclass
class PreparedTestcase:
    """
    A testcase that has been prepared for code generation.
    """

    testcase: Testcase
    "The original testcase."
    input: Union[MainInput, PreparedTestcaseStatement]
    "The prepared input from the testcase."
    exception_function: Callable[[Expression], Statement]
    "Function to handle exceptions."

    def exception_statement(self, name: Optional[str] = None) -> Statement:
        """
        Get the exception statement for the testcase.

        :param name: Optional, the name of a property containing the exception.
        :return: The exception statement.
        """
        if name:
            return self.exception_function(Identifier(name))
        else:
            return self.exception_function(NothingType())


@dataclass
class PreparedContext:
    """
    A context that has been prepared for code generation.
    """

    context: Context
    "The original context."
    testcases: List[PreparedTestcase]
    "A list of prepared testcases."
    before: str
    "The code to execute before the context."
    after: str
    "The code to execute after the context."


@dataclass
class PreparedExecutionUnit:
    """
    An execution unit that has been prepared for code generation.
    """

    execution_unit: "ExecutionUnit"
    "The original execution unit."
    contexts: List[PreparedContext]
    "The prepared contexts."
    execution_name: str
    "The name of the execution."
    value_file: str
    "The name of the file for the return channel."
    exception_file: str
    "The name of the file for the exception channel."
    submission_name: str
    "The name of the submission file."
    context_separator_secret: str
    "Secret for use in the context separator."
    testcase_separator_secret: str
    "Secret for use in the testcase separator."
    evaluator_names: Set[str]
    "The names of the language-specific evaluators we will need."


def generate_execution_unit(
    bundle: Bundle, prepared_execution: PreparedExecutionUnit
) -> str:
    """
    Generate the code for one execution unit.
    :param prepared_execution:
    :param bundle:
    :return:
    """
    return bundle.lang_config.generate_execution_unit(prepared_execution)


@dataclass
class PreparedFunctionCall(FunctionCall):
    has_root_namespace: bool = True
    # TODO: find out why this variable can't be initialized by the constructor


def _prepare_argument(
    bundle: Bundle, argument: Union[Expression, NamedArgument]
) -> Union[Expression, NamedArgument]:
    if isinstance(argument, NamedArgument):
        return NamedArgument(
            name=argument.name, value=_prepare_expression(bundle, argument.value)
        )
    return _prepare_expression(bundle, argument)


def _prepare_assignment(bundle: Bundle, assignment: Assignment) -> Assignment:
    if isinstance(assignment.type, VariableType):
        class_type = bundle.lang_config.conventionalize_class(assignment.type.data)
        assignment = assignment.replace_type(VariableType(data=class_type))

    assignment = assignment.replace_variable(
        bundle.lang_config.conventionalize_identifier(assignment.variable)
    )
    prepared = _prepare_expression(bundle, assignment.expression)
    return assignment.replace_expression(prepared)


def _prepare_expression(bundle: Bundle, expression: Expression) -> Expression:
    """
    Prepare an expression for use in a template.
    """

    if isinstance(expression, Identifier):
        expression = Identifier(
            bundle.lang_config.conventionalize_identifier(expression)
        )
    elif isinstance(expression, PreparedFunctionCall):
        expression.arguments = [
            _prepare_argument(bundle, arg) for arg in expression.arguments
        ]
    elif isinstance(expression, FunctionCall):
        submission_name = bundle.lang_config.submission_name(bundle.suite)
        if expression.type == FunctionType.CONSTRUCTOR:
            name = bundle.lang_config.conventionalize_class(expression.name)
        elif expression.type == FunctionType.PROPERTY:
            if expression.namespace is None:
                name = bundle.lang_config.conventionalize_global_identifier(
                    expression.name
                )
            else:
                name = bundle.lang_config.conventionalize_property(expression.name)
        else:
            name = bundle.lang_config.conventionalize_function(expression.name)

        if expression.namespace is None:
            namespace = Identifier(submission_name)
        else:
            namespace = _prepare_expression(bundle, expression.namespace)

        internal = PreparedFunctionCall(
            type=expression.type,
            arguments=[_prepare_argument(bundle, arg) for arg in expression.arguments],
            name=name,
            namespace=namespace,
        )
        internal.has_root_namespace = not bool(expression.namespace)
        return internal
    elif isinstance(expression, SequenceType):
        expression.data = [
            _prepare_expression(bundle, expr) for expr in expression.data
        ]
    elif isinstance(expression, ObjectType):
        expression.data = [
            ObjectKeyValuePair(
                key=_prepare_expression(bundle, pair.key),
                value=_prepare_expression(bundle, pair.value),
            )
            for pair in expression.data
        ]
    return expression


def _create_handling_function(
    bundle: Bundle,
    send_value: str,
    send_evaluated: str,
    output: Union[ExceptionOutput, ValueOutput],
) -> Tuple[Callable[[Expression], Statement], Optional[str]]:
    """
    Create a function to handle the result of a return value or an exception.

    There are two possibilities:
    - There is a language-specific evaluator. In that case, we wrap the value in
      a function call to the evaluator, and then send off the result. An example of
      the result:

        send_evaluated(evaluate(value))

    - There is no language-specific evaluator. In that case, we just send off the
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
    if hasattr(output, "evaluator") and isinstance(output.evaluator, SpecificEvaluator):
        # noinspection PyUnresolvedReferences
        evaluator = output.evaluator.for_language(bundle.config.programming_language)
        evaluator_name = lang_config.conventionalize_namespace(evaluator.file.stem)
    else:
        evaluator_name = None

    def generator(expression: Expression) -> Statement:
        if hasattr(output, "evaluator") and isinstance(
            output.evaluator, SpecificEvaluator
        ):
            arguments = [
                PreparedFunctionCall(
                    type=FunctionType.FUNCTION,
                    name=lang_config.conventionalize_function(evaluator.name),
                    namespace=Identifier(evaluator_name),
                    arguments=[_prepare_expression(bundle, expression)],
                )
            ]
            arguments[0].has_root_namespace = False
            function_name = send_evaluated
        else:
            arguments = [expression]
            function_name = send_value

        internal = PreparedFunctionCall(
            type=FunctionType.FUNCTION,
            name=lang_config.conventionalize_function(function_name),
            arguments=[_prepare_argument(bundle, arg) for arg in arguments],
        )
        internal.has_root_namespace = False
        return internal

    return generator, evaluator_name


def _create_exception_function(
    bundle: Bundle, testcase: Testcase
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
    bundle: Bundle, testcase: Testcase
) -> Tuple[PreparedTestcase, List[str]]:
    """
    Prepare a testcase.

    This will prepare any function calls or assignments, and extract functions
    for handling return values and exceptions. It also handles main calls.

    :param bundle: The configuration bundle.
    :param testcase: The testcase to prepare.

    :return: Arguments containing the preparation results and the evaluator name or
             None if no language-specific evaluator is needed.
    """
    names = []

    if testcase.is_main_testcase():
        prepared_input = testcase.input
    else:
        if isinstance(testcase.input, get_args(Expression)):
            # noinspection PyTypeChecker
            command = _prepare_expression(bundle, testcase.input)
        else:
            assert isinstance(testcase.input, get_args(Assignment))
            # noinspection PyTypeChecker
            command = _prepare_assignment(bundle, testcase.input)

        result_channel = testcase.output.result

        # Create the function to handle the values.
        value_function_call, evaluator_name = _create_handling_function(
            bundle, SEND_VALUE, SEND_SPECIFIC_VALUE, result_channel
        )
        if evaluator_name:
            names.append(evaluator_name)

        has_return = result_channel not in (EmptyChannel.NONE, IgnoredChannel.IGNORED)
        # A special case: if there isn't an actual value, don't call the function.
        if not has_return:
            value_function_call = None
            assert evaluator_name is None
        prepared_input = PreparedTestcaseStatement(
            statement=command, value_function=value_function_call
        )

    (exception_function_call, exception_evaluator_name) = _create_exception_function(
        bundle, testcase
    )

    if exception_evaluator_name:
        names.append(exception_evaluator_name)

    return (
        PreparedTestcase(
            testcase=testcase,
            input=prepared_input,
            exception_function=exception_function_call,
        ),
        names,
    )


def _prepare_testcases(
    bundle: Bundle, context: Context
) -> Tuple[List[PreparedTestcase], Set[str]]:
    """
    Prepare all testcases in a context.

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


def _handle_link_files(link_files: Iterable[FileUrl], language: str) -> Tuple[str, str]:
    dict_links = dict(
        (link_file.name, dataclasses.asdict(link_file)) for link_file in link_files
    )
    files = json.dumps(dict_links)
    return (
        f"<div class='contains-file highlight-{language} highlighter-rouge' "
        f"data-files={repr(files)}><pre style='padding: 2px; margin-bottom: "
        f"1px; background: none;'><code>",
        "</code></pre></div>",
    )


def _escape_shell(arg: str) -> str:
    # We want to always quote...
    quoted = shlex.quote(arg)
    if quoted.startswith("'") and quoted.endswith("'"):
        return quoted
    else:
        return f"'{quoted}'"


def get_readable_input(
    bundle: Bundle, files: List[FileUrl], case: Testcase
) -> Tuple[ExtendedMessage, Set[FileUrl]]:
    """
    Get human-readable input for a testcase. This function will use, in
    order of availability:

    1. A description on the testcase.
    2. If it is a normal testcase:
        a. A function expression or convert_statement.
    3. If it is a context testcase:
        a. The stdin and the arguments.
    """
    format_ = "text"  # By default, we use text as input.
    analyse_files = False
    if case.description:
        text = case.description
    elif case.is_main_testcase():
        # See https://rouge-ruby.github.io/docs/Rouge/Lexers/ConsoleLexer.html
        format_ = "console"
        arguments = " ".join(_escape_shell(x) for x in case.input.arguments)
        submission_name = bundle.lang_config.submission_name(bundle.suite)
        args = f"$ {submission_name} {arguments}"
        if isinstance(case.input.stdin, TextData):
            stdin = case.input.stdin.get_data_as_string(bundle.config.resources)
        else:
            stdin = ""
        if not stdin:
            text = args
            analyse_files = bool(arguments)
        else:
            if case.input.arguments:
                text = f"{args}\n{stdin}"
                analyse_files = True
            else:
                text = stdin
    else:
        format_ = bundle.config.programming_language
        # noinspection PyTypeChecker
        text = convert_statement(bundle, case.input)
        text = bundle.lang_config.cleanup_description(bundle.suite.namespace, text)
        analyse_files = True

    quote = bundle.lang_config.get_string_quote()
    if not analyse_files or not files:
        return ExtendedMessage(description=text, format=format_), set()
    if case.is_main_testcase():
        regex = re.compile("|".join(map(lambda x: re.escape(x.name), files)))
    else:
        regex = re.compile(
            f'{quote}{"|".join(map(lambda x: re.escape(x.name), files))}{quote}'
        )
    if not regex.search(text):
        return ExtendedMessage(description=text, format=format_), set()

    if format_ == "text":
        generated_html = html.escape(text)
    elif format_ == "console":
        generated_html = highlight_console(text)
    else:
        generator = bundle.lang_config.get_description_generator()
        generated_html = generator.generate_html_code(text)

    if case.is_main_testcase():
        regex = re.compile(
            f'({"|".join(map(lambda x: re.escape(html.escape(x.name)), files))})'
        )
        is_args = True
    else:
        quote = "&#39;" if quote == "'" else html.escape(quote)
        regex = re.compile(
            f"({quote})"
            f'({"|".join(map(lambda x: re.escape(html.escape(x.name)), files))})'
            f"({quote})"
        )
        is_args = False
    url_map = dict(map(lambda x: (html.escape(x.name), x), files))

    seen: Set[FileUrl] = set()

    def replace_link(match: Match) -> str:
        groups = match.groups()
        if is_args:
            file = url_map[groups[0]]
            seen.add(file)
            return (
                f'<a href={repr(file.url)} class="file-link" '
                f'target="_blank">{groups[0]}</a>'
            )
        file = url_map[groups[1]]
        seen.add(file)
        return (
            f'{groups[0]}<a href={repr(file.url)} class="file-link" '
            f'target="_blank">{groups[1]}</a>{groups[2]}'
        )

    generated_html = regex.sub(replace_link, generated_html)
    prefix, suffix = _handle_link_files(seen, format_)
    generated_html = f"{prefix}{generated_html}{suffix}"
    return ExtendedMessage(description=generated_html, format="html"), seen


def attempt_run_readable_input(bundle: Bundle, run: Testcase) -> ExtendedMessage:
    result, _ = get_readable_input(bundle, run.link_files, run)
    if result.description:
        return result

    return ExtendedMessage(
        description=get_i18n_string("languages.generator.missing.input"), format="text"
    )


def attempt_readable_input(bundle: Bundle, context: Context) -> ExtendedMessage:
    # Try until we find a testcase with input.
    testcases = context.testcases
    for testcase in testcases:
        result, _ = get_readable_input(bundle, context.link_files, testcase)
        if result.description:
            return result

    return ExtendedMessage(
        description=get_i18n_string("languages.generator.missing.input"), format="text"
    )


def convert_statement(bundle: Bundle, statement: Statement) -> str:
    """
    Convert a statement to actual code for the given programming language.
    This will use the "full" mode, meaning variable_type annotation will be
    present. For
    example, in "default" mode, Java code will look like this:

        test = namespace.functionCall("Hi");

    In full mode, this will be:

        String test = namespace.functionCall("Hi");

    :param bundle: The configuration bundle.
    :param statement: The statement to convert.

    :return: The code the statement.
    """
    if isinstance(statement, get_args(Expression)):
        statement = _prepare_expression(bundle, statement)
    else:
        assert isinstance(statement, get_args(Assignment))
        statement = _prepare_assignment(bundle, statement)

    return bundle.lang_config.generate_statement(statement)


def _prepare_context(
    bundle: Bundle, context: Context
) -> Tuple[PreparedContext, Set[str]]:
    """
    Prepare one context for the execution

    :param bundle: The configuration bundle.
    :param context: The context to prepare

    :return: The prepared context arguments and a set
             of evaluator names.
    """
    language = bundle.config.programming_language
    resources = bundle.config.resources
    before_code = context.before.get(language, TextData(data="")).get_data_as_string(
        resources
    )
    after_code = context.after.get(language, TextData(data="")).get_data_as_string(
        resources
    )
    testcases, evaluator_names = _prepare_testcases(bundle, context)
    return (
        PreparedContext(
            before=before_code, after=after_code, testcases=testcases, context=context
        ),
        evaluator_names,
    )


# execution_args = prepare_execution_unit(
#     bundle, destination, execution_name, execution_unit
# )
#
# evaluator_files = [f"{x}.{bundle.lang_config.extension_file()}" for x in
#                    execution_args.evaluator_names]
#
# execution_destination = destination / bundle.lang_config.with_extension(
#     execution_name)
# template = bundle.lang_config.template_name(TemplateType.RUN)
#
# return (
#     find_and_write_template(
#         bundle, execution_args, execution_destination, template
#     ),
#     evaluator_files,
# )


def prepare_execution_unit(
    bundle: Bundle,
    destination: Path,
    execution_name: str,
    execution_unit: "ExecutionUnit",
) -> PreparedExecutionUnit:
    """
    Prepare an execution unit for code generation.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param execution_unit: The execution for which generation is happening.
    :param execution_name: The name of the execution module.

    :return: The name of the generated file in the given destination and a set
             of evaluator names that will also be needed.
    """
    evaluator_names = set()
    contexts = []
    for context in execution_unit.contexts:
        context_args, context_evaluator_names = _prepare_context(bundle, context)
        contexts.append(context_args)
        evaluator_names.update(context_evaluator_names)

    value_file_name = value_file(bundle, destination).name
    exception_file_name = exception_file(bundle, destination).name
    submission_name = bundle.lang_config.submission_name(bundle.suite)

    # Extract the main testcase and exit testcase.

    return PreparedExecutionUnit(
        execution_name=execution_name,
        value_file=value_file_name,
        exception_file=exception_file_name,
        submission_name=submission_name,
        testcase_separator_secret=bundle.testcase_separator_secret,
        context_separator_secret=bundle.context_separator_secret,
        contexts=contexts,
        execution_unit=execution_unit,
        evaluator_names=evaluator_names,
    )


def generate_execution(
    bundle: Bundle,
    destination: Path,
    execution_unit: "ExecutionUnit",
    execution_name: str,
) -> Tuple[str, List[str]]:
    """
    Generate the files related to the execution.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param execution_unit: The execution for which generation is happening.
    :param execution_name: The name of the execution module.

    :return: The name of the generated file in the given destination and a set
             of evaluator names that will also be needed.
    """
    prepared_execution = prepare_execution_unit(
        bundle, destination, execution_name, execution_unit
    )

    execution_code = generate_execution_unit(bundle, prepared_execution)

    evaluator_files = [
        f"{x}.{bundle.lang_config.extension_file()}"
        for x in prepared_execution.evaluator_names
    ]

    execution_name = bundle.lang_config.with_extension(execution_name)
    execution_destination = destination / execution_name

    with open(execution_destination, "w") as execution_file:
        execution_file.write(execution_code)

    return execution_name, evaluator_files


def generate_selector(
    bundle: Bundle, destination: Path, context_names: List[str]
) -> str:
    """
    Generate the file to execute_module a single context.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param context_names: The names of the contexts.

    :return: The name of the generated file in the given destination.
    """
    assert bundle.lang_config.needs_selector()
    selector_name = bundle.lang_config.selector_name()
    selector_filename = bundle.lang_config.with_extension(selector_name)
    selector_destination = destination / selector_filename
    selector_code = bundle.lang_config.generate_selector(context_names)
    with open(selector_destination, "w") as execution_file:
        execution_file.write(selector_code)
    return selector_filename


def custom_evaluator_arguments(evaluator: ProgrammedEvaluator) -> Value:
    return SequenceType(type=BasicSequenceTypes.SEQUENCE, data=evaluator.arguments)


def generate_custom_evaluator(
    bundle: Bundle,
    destination: Path,
    evaluator: ProgrammedEvaluator,
    expected_value: Value,
    actual_value: Value,
) -> str:
    """
    Generate the code for running a programmed evaluator.

    :param bundle: The configuration bundle.
    :param destination: The folder where the code should be generated.
    :param evaluator: The evaluator data from the test suite.
    :param expected_value: The preprocessed expected value.
    :param actual_value: The preprocessed actual value.

    :return: The name of the generated file.
    """
    evaluator_name = bundle.lang_config.conventionalize_namespace(
        evaluator.function.file.stem
    )
    arguments = custom_evaluator_arguments(evaluator)

    function = PreparedFunctionCall(
        type=FunctionType.FUNCTION,
        namespace=Identifier(evaluator_name),
        name=evaluator.function.name,
        arguments=[expected_value, actual_value, arguments],
    )
    function.has_root_namespace = False

    code = bundle.lang_config.generate_check_function(evaluator_name, function)

    if destination.is_dir():
        destination /= bundle.lang_config.with_extension("EvaluatorExecutor")

    # noinspection PyTypeChecker
    with open(destination, "w") as check_function_file:
        check_function_file.write(code)

    return destination.name


def value_file(bundle: Bundle, directory: Path):
    """
    Return the path to the value file. The file will be placed inside the given
    working directory.

    :param bundle: The configuration bundle.
    :param directory: The directory in which to place the file.

    :return: The path to the file, depending on the working directory.
    """
    return directory / f"{bundle.testcase_separator_secret}_values.txt"


def exception_file(bundle: Bundle, directory: Path):
    """
    Return the path to the exception file. The file will be placed inside the given
    working directory.

    :param bundle: The configuration bundle.
    :param directory: The directory in which to place the file.

    :return: The path to the file, depending on the working directory.
    """
    return directory / f"{bundle.testcase_separator_secret}_exceptions.txt"
