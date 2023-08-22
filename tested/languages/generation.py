"""
Translates items from the test suite into the actual programming language.
"""
import html
import json
import logging
import re
import shlex
from pathlib import Path
from typing import TYPE_CHECKING, Iterable, List, Match, Set, Tuple, TypeAlias

from pygments import highlight
from pygments.formatters.html import HtmlFormatter
from pygments.lexers import get_lexer_by_name

from tested.configs import Bundle
from tested.datatypes import AllTypes, BasicObjectTypes, BasicSequenceTypes
from tested.dodona import ExtendedMessage
from tested.internationalization import get_i18n_string
from tested.languages import Language
from tested.languages.conventionalize import (
    conventionalize_namespace,
    selector_file,
    submission_name,
)
from tested.languages.preparation import (
    PreparedExecutionUnit,
    PreparedFunctionCall,
    prepare_assignment,
    prepare_execution_unit,
    prepare_expression,
)
from tested.parsing import get_converter
from tested.serialisation import (
    Assignment,
    Expression,
    FunctionType,
    Identifier,
    SequenceType,
    Statement,
    Value,
    VariableType,
)
from tested.testsuite import (
    Context,
    CustomCheckOracle,
    FileUrl,
    LanguageLiterals,
    MainInput,
    Testcase,
    TextData,
)

# Prevent cyclic imports for types...
if TYPE_CHECKING:
    from tested.judge.execution import ExecutionUnit

_logger = logging.getLogger(__name__)
_html_formatter = HtmlFormatter(nowrap=True)


# Alias for type declarations
NestedTypeDeclaration: TypeAlias = (
    AllTypes | Tuple[AllTypes, Tuple["NestedTypeDeclaration", ...]]
)


def highlight_code(stmt: str, language: str = "console") -> str:
    """
    Highlight code for some programming language.

    :param stmt: The code to highlight.
    :param language: The language of the code.
    """
    lexer = get_lexer_by_name(language, stripall=True)
    return highlight(stmt, lexer, _html_formatter)


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


def _handle_link_files(link_files: Iterable[FileUrl], language: str) -> Tuple[str, str]:
    dict_links = dict(
        (link_file.name, get_converter().unstructure(link_file))
        for link_file in link_files
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
        a. A function expression or generate_statement.
    3. If it is a context testcase:
        a. The stdin and the arguments.
    """
    format_ = "text"  # By default, we use text as input.
    analyse_files = False
    if case.description:
        text = case.description
    elif case.is_main_testcase():
        assert isinstance(case.input, MainInput)
        # See https://rouge-ruby.github.io/docs/Rouge/Lexers/ConsoleLexer.html
        format_ = "console"
        arguments = " ".join(_escape_shell(x) for x in case.input.arguments)
        submission = submission_name(bundle.lang_config)
        args = f"$ {submission} {arguments}"
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
    elif isinstance(case.input, Statement):
        format_ = bundle.config.programming_language
        text = generate_statement(bundle, case.input)
        text = bundle.lang_config.cleanup_description(text)
        analyse_files = True
    else:
        assert isinstance(case.input, LanguageLiterals)
        text = case.input.get_for(bundle.config.programming_language)

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
        generated_html = highlight_code(text)
    else:
        generated_html = highlight_code(text, bundle.config.programming_language)

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


def generate_statement(bundle: Bundle, statement: Statement) -> str:
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
    if isinstance(statement, Expression):
        statement = prepare_expression(bundle, statement)
    else:
        assert isinstance(statement, Assignment)
        statement = prepare_assignment(bundle, statement)

    return bundle.lang_config.generate_statement(statement)


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
             of oracle names that will also be needed.
    """
    prepared_execution = prepare_execution_unit(
        bundle, destination, execution_name, execution_unit
    )

    execution_code = generate_execution_unit(bundle, prepared_execution)

    evaluator_files = [
        f"{x}.{bundle.lang_config.file_extension()}"
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
    selector_filename = selector_file(bundle.lang_config)
    selector_destination = destination / selector_filename
    selector_code = bundle.lang_config.generate_selector(context_names)
    with open(selector_destination, "w") as execution_file:
        execution_file.write(selector_code)
    return selector_filename


def custom_oracle_arguments(oracle: CustomCheckOracle) -> Value:
    return SequenceType(
        type=BasicSequenceTypes.SEQUENCE, data=oracle.arguments  # pyright: ignore
    )


def generate_custom_evaluator(
    bundle: Bundle,
    destination: Path,
    evaluator: CustomCheckOracle,
    expected_value: Value,
    actual_value: Value,
) -> str:
    """
    Generate the code for running a programmed oracle.

    :param bundle: The configuration bundle.
    :param destination: The folder where the code should be generated.
    :param evaluator: The oracle data from the test suite.
    :param expected_value: The preprocessed expected value.
    :param actual_value: The preprocessed actual value.

    :return: The name of the generated file.
    """
    evaluator_name = conventionalize_namespace(
        bundle.lang_config, evaluator.function.file.stem
    )
    arguments = custom_oracle_arguments(evaluator)

    function = PreparedFunctionCall(
        type=FunctionType.FUNCTION,
        namespace=Identifier(evaluator_name),
        name=evaluator.function.name,
        arguments=[expected_value, actual_value, arguments],
        has_root_namespace=False,
    )

    code = bundle.lang_config.generate_check_function(evaluator_name, function)

    if destination.is_dir():
        destination /= bundle.lang_config.with_extension("EvaluatorExecutor")

    with open(destination, "w") as check_function_file:
        check_function_file.write(code)

    return destination.name


def _convert_single_type(
    language: Language, type_: AllTypes | VariableType, inner: bool
) -> str | bool:
    if isinstance(type_, VariableType):
        return type_.data
    meta = language.get_declaration_metadata()
    if inner and type_ in meta.get("inner_names", {}):
        return meta["inner_names"][type_]  # type: ignore
    else:
        return meta["names"].get(type_, type_)


def generate_type_declaration(
    language: Language, declaration: NestedTypeDeclaration, inner: bool = False
) -> str | bool:
    if not isinstance(declaration, tuple):
        return _convert_single_type(language, declaration, inner)

    meta = language.get_declaration_metadata()
    base_type, nested = declaration
    base = _convert_single_type(language, base_type, inner)

    start, finish = meta.get("nested", ("[", "]"))

    if base_type in meta.get("nested_overrides", {}):
        start, finish = meta["nested_overrides"][base_type]  # type: ignore

    if isinstance(base_type, BasicObjectTypes):
        assert (
            len(nested) == 2
        ), "Object types require exactly two nested types if present."

    converted_nested = []
    for x in nested:
        converted_x = generate_type_declaration(language, x, True)
        assert isinstance(converted_x, str)
        converted_nested.append(converted_x)

    if base is True:
        # This is a type with "inner" types, i.e. no name, such as Tuple[bool]
        return f"{start}{', '.join(converted_nested)}{finish}"
    elif base is False:
        # This is a type with "rigt" types, i.e. no name, such as bool[]
        return f"{', '.join(converted_nested)}{start}{finish}"
    else:
        return f"{base}{start}{', '.join(converted_nested)}{finish}"
