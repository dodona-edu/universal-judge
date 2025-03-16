"""
Translates items from the test suite into the actual programming language.
"""

import html
import json
import logging
import re
import shlex
import urllib.parse
from collections.abc import Iterable
from pathlib import Path
from re import Match
from typing import TYPE_CHECKING, TypeAlias

from pygments import highlight
from pygments.formatters.html import HtmlFormatter
from pygments.lexers import get_lexer_by_name

from tested.configs import Bundle
from tested.datatypes import AllTypes, BasicObjectTypes
from tested.dodona import ExtendedMessage
from tested.internationalization import get_i18n_string
from tested.languages import Language
from tested.languages.conventionalize import selector_file, submission_name
from tested.languages.preparation import (
    PreparedExecutionUnit,
    prepare_assignment,
    prepare_execution_unit,
    prepare_expression,
)
from tested.parsing import get_converter
from tested.serialisation import Expression, Statement, VariableType
from tested.testsuite import (
    Context,
    FileUrl,
    LanguageLiterals,
    MainInput,
    Testcase,
    TextData,
)
from tested.utils import is_statement_strict

if TYPE_CHECKING:
    from tested.judge.planning import PlannedExecutionUnit

_logger = logging.getLogger(__name__)
_html_formatter = HtmlFormatter(nowrap=True)


# Alias for type declarations
NestedTypeDeclaration: TypeAlias = (
    AllTypes | tuple[AllTypes, tuple["NestedTypeDeclaration", ...]]
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
    return bundle.language.generate_execution_unit(prepared_execution)


def _handle_link_files(link_files: Iterable[FileUrl], language: str) -> tuple[str, str]:
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


def _get_heredoc_token(stdin: str) -> str:
    delimiter = "STDIN"
    while delimiter in stdin:
        delimiter = delimiter + "N"
    return delimiter


def get_readable_input(
    bundle: Bundle, case: Testcase
) -> tuple[ExtendedMessage, set[FileUrl]]:
    """
    Get human-readable input for a testcase. This function will use, in
    order of availability:

    1. A description on the testcase.
    2. If it is a normal testcase:
        a. A function expression or generate_statement.
    3. If it is a context testcase:
        a. The stdin and the arguments.
    """
    # We have potential files.
    # Check if the file names are present in the string.
    # If not, we can also stop before doing ugly things.
    # We construct a regex, since that can be faster than checking everything.
    simple_regex = re.compile(
        "|".join(map(lambda x: re.escape(x.name), case.link_files))
    )

    format_ = "text"  # By default, we use text as input.
    if case.description:
        if isinstance(case.description, ExtendedMessage):
            text = case.description.description
            format_ = case.description.format
        else:
            text = case.description
    elif case.is_main_testcase():
        assert isinstance(case.input, MainInput)
        # See https://rouge-ruby.github.io/docs/Rouge/Lexers/ConsoleLexer.html
        format_ = "console"
        # Determine the command (with arguments)
        submission = submission_name(bundle.language)
        command = shlex.join([submission] + case.input.arguments)
        args = f"$ {command}"
        # Determine the stdin
        if isinstance(case.input.stdin, TextData):
            stdin = case.input.stdin.data
            if not case.link_files and not simple_regex.search(stdin):
                stdin = case.input.stdin.get_data_as_string(bundle.config.resources)
        else:
            stdin = ""

        # If we have both stdin and arguments, we use a here-document.
        if case.input.arguments and stdin:
            assert stdin[-1] == "\n", "stdin must end with a newline"
            delimiter = _get_heredoc_token(stdin)
            text = f"{args} << '{delimiter}'\n{stdin}{delimiter}"
        elif stdin:
            assert not case.input.arguments
            text = stdin
        else:
            text = args
    elif isinstance(case.input, Statement):
        format_ = bundle.config.programming_language
        text = generate_statement(bundle, case.input)
        text = bundle.language.cleanup_description(text)

        if case.line_comment:
            text = f"{text} {bundle.language.comment(case.line_comment)}"
    else:
        assert isinstance(case.input, LanguageLiterals)
        text = case.input.get_for(bundle.config.programming_language)
        format_ = bundle.config.programming_language

        if case.line_comment:
            text = f"{text} {bundle.language.comment(case.line_comment)}"

    # If there are no files, return now. This means we don't need to do ugly stuff.
    if not case.link_files:
        return ExtendedMessage(description=text, format=format_), set()


    if not simple_regex.search(text):
        # There is no match, so bail now.
        return ExtendedMessage(description=text, format=format_), set()

    # Now we need to do ugly stuff.
    # Begin by compiling the HTML that will be displayed.
    if format_ == "text":
        generated_html = html.escape(text)
    elif format_ == "console":
        generated_html = highlight_code(text)
    else:
        generated_html = highlight_code(text, bundle.config.programming_language)

    # Map of file URLs.
    url_map = {html.escape(x.name): x for x in case.link_files}

    seen = set()
    escaped_regex = re.compile("|".join(url_map.keys()))

    # Replaces the match with the corresponding link.
    def replace_link(match: Match) -> str:
        filename = match.group()
        the_file = url_map[filename]
        the_url = urllib.parse.quote(the_file.url)
        the_replacement = (
            f'<a href="{the_url}" class="file-link" target="_blank">{filename}</a>'
        )
        seen.add(the_file)
        return the_replacement

    generated_html = escaped_regex.sub(replace_link, generated_html)
    prefix, suffix = _handle_link_files(seen, format_)
    generated_html = f"{prefix}{generated_html}{suffix}"
    return ExtendedMessage(description=generated_html, format="html"), seen


def attempt_readable_input(bundle: Bundle, context: Context) -> ExtendedMessage:
    # Try until we find a testcase with input.
    testcases = context.testcases
    for testcase in testcases:
        result, _ = get_readable_input(bundle, testcase)
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
        assert is_statement_strict(statement)
        statement = prepare_assignment(bundle, statement)

    return bundle.language.generate_statement(statement)


def generate_execution(
    bundle: Bundle,
    destination: Path,
    execution_unit: "PlannedExecutionUnit",
) -> tuple[str, list[str]]:
    """
    Generate the files related to the execution.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param execution_unit: The execution for which generation is happening.

    :return: The name of the generated file in the given destination and a set
             of oracle names that will also be needed.
    """
    prepared_execution = prepare_execution_unit(bundle, destination, execution_unit)
    execution_code = generate_execution_unit(bundle, prepared_execution)

    evaluator_files = [
        f"{x}.{bundle.language.file_extension()}"
        for x in prepared_execution.evaluator_names
    ]

    execution_name = bundle.language.with_extension(execution_unit.name)
    execution_destination = destination / execution_name

    with open(execution_destination, "w") as execution_file:
        execution_file.write(execution_code)

    return execution_name, evaluator_files


def generate_selector(
    bundle: Bundle, destination: Path, context_names: list[str]
) -> str:
    """
    Generate the file to execute_module a single context.

    :param bundle: The configuration bundle.
    :param destination: Where the generated files should go.
    :param context_names: The names of the contexts.

    :return: The name of the generated file in the given destination.
    """
    assert bundle.language.needs_selector()
    selector_filename = selector_file(bundle.language)
    selector_destination = destination / selector_filename
    selector_code = bundle.language.generate_selector(context_names)
    with open(selector_destination, "w") as execution_file:
        execution_file.write(selector_code)
    return selector_filename


def _convert_single_type(
    language: Language, type_: AllTypes | VariableType, inner: bool
) -> str | tuple[bool, str]:
    if isinstance(type_, VariableType):
        return type_.data
    meta = language.get_declaration_metadata()
    if inner and type_ in meta.get("inner_names", {}):
        return meta["inner_names"][type_]  # type: ignore
    else:
        return meta["names"].get(type_, type_)


def generate_type_declaration(
    language: Language, declaration: NestedTypeDeclaration, inner: bool = False
) -> str:
    if not isinstance(declaration, tuple):
        simple_result = _convert_single_type(language, declaration, inner)
        if isinstance(simple_result, tuple):
            simple_result = simple_result[1]
        assert isinstance(
            simple_result, str
        ), f"{declaration} is a simple type and should generate a string"
        return simple_result

    meta = language.get_declaration_metadata()
    base_type, nested = declaration
    base = _convert_single_type(language, base_type, inner)
    if isinstance(base, tuple):
        base = base[0]

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
        converted_nested.append(converted_x)

    if base is True:
        # This is a type with "inner" types, i.e. no name, such as Tuple[bool]
        return f"{start}{', '.join(converted_nested)}{finish}"
    elif base is False:
        # This is a type with "right" types, i.e. no name, such as bool[]
        return f"{', '.join(converted_nested)}{start}{finish}"
    else:
        return f"{base}{start}{', '.join(converted_nested)}{finish}"
