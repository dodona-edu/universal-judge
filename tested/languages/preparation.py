"""
Prepare test suite input for code generation.

Most input from a test suite needs to be prepared to easily generated code. This
module handles that.
"""
from pathlib import Path
from typing import TYPE_CHECKING, Callable, List, Optional, Set, Tuple, Union, cast

from attrs import define

from tested.configs import Bundle
from tested.languages.conventionalize import (
    conventionalize_class,
    conventionalize_function,
    conventionalize_global_identifier,
    conventionalize_identifier,
    conventionalize_namespace,
    conventionalize_property,
    submission_name,
)
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
    VariableType,
)
from tested.testsuite import (
    Context,
    EmptyChannel,
    ExceptionOutput,
    IgnoredChannel,
    LanguageSpecificOracle,
    MainInput,
    OracleOutputChannel,
    Testcase,
    TextData,
    ValueOutput,
)

if TYPE_CHECKING:
    from tested.judge.execution import ExecutionUnit

# Names of the predefined functions that must be available.
SEND_VALUE = "send_value"
SEND_EXCEPTION = "send_exception"
SEND_SPECIFIC_VALUE = "send_specific_value"
SEND_SPECIFIC_EXCEPTION = "send_specific_exception"


@define
class PreparedFunctionCall(FunctionCall):
    has_root_namespace: bool = True
    # TODO: find out why this variable can't be initialized by the constructor


@define
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
            assert isinstance(self.statement, Expression)
            if override:
                return self.value_function(Identifier(override))
            else:
                return self.value_function(self.statement)
        else:
            return self.statement


@define
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


@define
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


@define
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
    "The names of the language-specific functions we will need."


def prepare_argument(
    bundle: Bundle, argument: Union[Expression, NamedArgument]
) -> Union[Expression, NamedArgument]:
    if isinstance(argument, NamedArgument):
        return NamedArgument(
            name=argument.name, value=prepare_expression(bundle, argument.value)
        )
    return prepare_expression(bundle, argument)


def prepare_assignment(bundle: Bundle, assignment: Assignment) -> Assignment:
    if isinstance(assignment.type, VariableType):
        class_type = conventionalize_class(bundle.lang_config, assignment.type.data)
        assignment = assignment.replace_type(VariableType(data=class_type))

    assignment = assignment.replace_variable(
        conventionalize_identifier(bundle.lang_config, assignment.variable)
    )
    prepared = prepare_expression(bundle, assignment.expression)
    return assignment.replace_expression(prepared)


def prepare_expression(bundle: Bundle, expression: Expression) -> Expression:
    """
    Prepare an expression for use in a template.
    """

    if isinstance(expression, Identifier):
        expression = Identifier(
            conventionalize_identifier(bundle.lang_config, expression)
        )
    elif isinstance(expression, PreparedFunctionCall):
        expression.arguments = [
            prepare_argument(bundle, arg) for arg in expression.arguments
        ]
    elif isinstance(expression, FunctionCall):
        submission = submission_name(bundle.lang_config)
        if expression.type == FunctionType.CONSTRUCTOR:
            name = conventionalize_class(bundle.lang_config, expression.name)
        elif expression.type == FunctionType.PROPERTY:
            if expression.namespace is None:
                name = conventionalize_global_identifier(
                    bundle.lang_config, expression.name
                )
            else:
                name = conventionalize_property(bundle.lang_config, expression.name)
        else:
            name = conventionalize_function(bundle.lang_config, expression.name)

        if expression.namespace is None:
            namespace = Identifier(submission)
        else:
            namespace = prepare_expression(bundle, expression.namespace)

        internal = PreparedFunctionCall(
            type=expression.type,
            arguments=[prepare_argument(bundle, arg) for arg in expression.arguments],
            name=name,
            namespace=namespace,
        )
        internal.has_root_namespace = not bool(expression.namespace)
        return internal
    elif isinstance(expression, SequenceType):
        expression.data = [prepare_expression(bundle, expr) for expr in expression.data]
    elif isinstance(expression, ObjectType):
        expression.data = [
            ObjectKeyValuePair(
                key=prepare_expression(bundle, pair.key),
                value=prepare_expression(bundle, pair.value),
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
    - There is a language-specific oracle. In that case, we wrap the value in
      a function call to the oracle, and then send off the result. An example of
      the result:

        send_evaluated(evaluate(value))

    - There is no language-specific oracle. In that case, we just send off the
      value directly. An example of the result:

        send_value(value)

    :param bundle: The configuration bundle.
    :param send_evaluated: The name of the function that will handle sending the
                           result of an evaluation.
    :param send_value: The name of the function that will handle sending the value.
    :param output: The oracle.
    :return: A tuple containing the call and the name of the oracle if present.
    """
    lang_config = bundle.lang_config
    if isinstance(output, OracleOutputChannel) and isinstance(
        output.oracle, LanguageSpecificOracle
    ):
        evaluator = output.oracle.for_language(bundle.config.programming_language)
        evaluator_name = conventionalize_namespace(lang_config, evaluator.file.stem)
    else:
        evaluator_name = None

    def generator(expression: Expression) -> Statement:
        if isinstance(output, OracleOutputChannel) and isinstance(
            output.oracle, LanguageSpecificOracle
        ):
            assert evaluator
            arguments = [
                PreparedFunctionCall(
                    type=FunctionType.FUNCTION,
                    name=conventionalize_function(lang_config, evaluator.name),
                    namespace=Identifier(evaluator_name),
                    arguments=[prepare_expression(bundle, expression)],
                )
            ]
            arguments[0].has_root_namespace = False
            function_name = send_evaluated
        else:
            arguments = [expression]
            function_name = send_value

        internal = PreparedFunctionCall(
            type=FunctionType.FUNCTION,
            name=conventionalize_function(lang_config, function_name),
            arguments=[prepare_argument(bundle, arg) for arg in arguments],
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

    :return: The function and optionally the name of the oracle file.
    """
    # If we have a regular testcase, handle special functions.

    exception_channel = testcase.output.exception
    return _create_handling_function(
        bundle, SEND_EXCEPTION, SEND_SPECIFIC_EXCEPTION, exception_channel
    )


def prepare_testcase(
    bundle: Bundle, testcase: Testcase
) -> Tuple[PreparedTestcase, List[str]]:
    """
    Prepare a testcase.

    This will prepare any function calls or assignments, and extract functions
    for handling return values and exceptions. It also handles main calls.

    :param bundle: The configuration bundle.
    :param testcase: The testcase to prepare.

    :return: Arguments containing the preparation results and the oracle name or
             None if no language-specific oracle is needed.
    """
    names = []

    if testcase.is_main_testcase():
        prepared_input = cast(MainInput, testcase.input)
    else:
        if isinstance(testcase.input, Expression):
            command = prepare_expression(bundle, testcase.input)
        else:
            assert isinstance(testcase.input, Assignment)
            command = prepare_assignment(bundle, testcase.input)

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


def prepare_testcases(
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
        args, new_names = prepare_testcase(bundle, testcase)
        result.append(args)
        files.update(new_names)
    return result, files


def prepare_context(
    bundle: Bundle, context: Context
) -> Tuple[PreparedContext, Set[str]]:
    """
    Prepare one context for the execution

    :param bundle: The configuration bundle.
    :param context: The context to prepare

    :return: The prepared context arguments and a set
             of oracle names.
    """
    language = bundle.config.programming_language
    resources = bundle.config.resources
    before_code = context.before.get(language, TextData(data="")).get_data_as_string(
        resources
    )
    after_code = context.after.get(language, TextData(data="")).get_data_as_string(
        resources
    )
    testcases, evaluator_names = prepare_testcases(bundle, context)
    return (
        PreparedContext(
            before=before_code, after=after_code, testcases=testcases, context=context
        ),
        evaluator_names,
    )


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
             of oracle names that will also be needed.
    """
    evaluator_names = set()
    contexts = []
    for context in execution_unit.contexts:
        context_args, context_evaluator_names = prepare_context(bundle, context)
        contexts.append(context_args)
        evaluator_names.update(context_evaluator_names)

    value_file_name = value_file(bundle, destination).name
    exception_file_name = exception_file(bundle, destination).name
    submission = submission_name(bundle.lang_config)

    return PreparedExecutionUnit(
        execution_name=execution_name,
        value_file=value_file_name,
        exception_file=exception_file_name,
        submission_name=submission,
        testcase_separator_secret=bundle.testcase_separator_secret,
        context_separator_secret=bundle.context_separator_secret,
        contexts=contexts,
        execution_unit=execution_unit,
        evaluator_names=evaluator_names,
    )
