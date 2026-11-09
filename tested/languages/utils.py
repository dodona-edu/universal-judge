import html
import logging
import os
import re
from pathlib import Path
from typing import TYPE_CHECKING, overload

from tested.configs import DodonaConfig, GlobalConfig
from tested.datatypes import BasicStringTypes
from tested.dodona import ExtendedMessage, Permission
from tested.languages.conventionalize import submission_name
from tested.languages.preparation import PreparedTestcaseStatement
from tested.serialisation import FunctionCall, StringType

if TYPE_CHECKING:
    from tested.languages.language import Language

_logger = logging.getLogger(__name__)
code_regex = re.compile(r"<code>:(\d+)")
gcc_gutter_regex = re.compile(r"^( *)(\d+)( \|)")


def workdir_sources(lang_config: Language) -> list[str]:
    """The top-level workdir files in the language's extension, sorted."""
    assert lang_config.config
    workdir = lang_config.config.dodona.workdir
    sources = workdir.glob(f"*.{lang_config.file_extension()}")
    return sorted(str(f.absolute()) for f in sources if f.is_file())


def cleanup_description(lang_config: Language, description: str) -> str:
    return description.replace(rf"{submission_name(lang_config)}.", r"", 1)


def jvm_memory_limit(config: GlobalConfig) -> int:
    """
    Get the memory limit in bytes. Java Virtual Machine (JVM) requires this to be a
    multiple of 1024.
    See https://docs.oracle.com/en/java/javase/14/docs/specs/man/java.html
    """
    limit = int(config.dodona.memory_limit)
    limit = (limit // 1024) * 1024
    return limit


# Idea and original code: dodona/judge-pythia
def jvm_cleanup_stacktrace(stacktrace_str: str, submission_filename: str) -> str:
    context_file_regex = re.compile(r"(Context[0-9]+|Selector)")
    execution_regex = re.compile(r"Execution(\d+)\.kt")
    generated_kotlin_regex = re.compile(
        r"^\s*(?:\S*/)?(?:Execution\d+|Context\d+|Selector)\.kt:\d+:\d+: (error|warning): (.*)$"
    )
    stacktrace = stacktrace_str.splitlines(True)

    skip_line, in_generated_diagnostic = False, False
    lines, generated_errors = [], []
    for line in stacktrace:
        line = line.strip("\n")

        if not line:
            continue

        # skip the snippet and caret lines below a diagnostic in a generated file
        if (
            in_generated_diagnostic
            and line.startswith(" ")
            and submission_filename not in line
        ):
            continue
        in_generated_diagnostic = False

        if match := generated_kotlin_regex.match(line):
            if match[1] == "error":
                message = match[2].replace("solutionMain", "main")
                generated_errors.append(f"error: {message}\n")
            in_generated_diagnostic = True
            continue

        # skip line if not a new File line is started
        if context_file_regex.search(line):
            skip_line = True
            continue
        elif skip_line and (line.startswith(" ") and "at" not in line):
            continue

        # replace references to local names
        if submission_filename in line:
            line = line.replace(submission_filename, "<code>")
            line = line.replace("Kt.solutionMain", "Kt.main")
            line = line.replace("SubmissionKt.solutionMain", "SubmissionKt.main")
        elif "at " in line:
            skip_line = True
            continue

        if execution_regex.search(line):
            skip_line = True
            continue

        skip_line = False
        lines.append(line + "\n")

    # Errors in generated files are often fallout of an error in the submission.
    if not any("<code>" in line and "error:" in line for line in lines):
        lines.extend(generated_errors)

    return "".join(lines)


_haskell_header_skip = re.compile(
    r"\s+|--[^\n]*|(?P<pragma>\{-#.*?#-\})|\{-.*?-\}", re.DOTALL
)
_haskell_module_header = re.compile(r"module(\s+)[\w.']+")


def haskell_add_module_header(contents: str, name: str) -> tuple[str, bool]:
    """
    Make the file a module called `name`.

    Adds a `module <name> where` line after the pragmas at the top of the file, or
    renames the module if the file declares its own. Returns the new contents and
    whether a line was added.
    """
    position = 0
    pragmas_end = 0
    while match := _haskell_header_skip.match(contents, position):
        position = match.end()
        if match.group("pragma"):
            pragmas_end = position
    if header := _haskell_module_header.match(contents, position):
        return (
            f"{contents[:position]}module{header.group(1)}{name}{contents[header.end() :]}",
            False,
        )
    if pragmas_end == 0:
        return f"module {name} where\n{contents}", True
    line_end = contents.find("\n", pragmas_end)
    if line_end == -1:
        return f"{contents}\nmodule {name} where", True
    split = line_end + 1
    return f"{contents[:split]}module {name} where\n{contents[split:]}", True


def haskell_solution(lang_config: Language, solution: Path):
    """Support implicit modules if needed."""
    assert lang_config.config
    if lang_config.config.dodona.config_for().get("implicitModule", True):
        contents, added = haskell_add_module_header(
            solution.read_text(), submission_name(lang_config)
        )
        solution.write_text(contents)
        if added:
            lang_config.config.dodona.source_offset -= 1


def executable_name(basename: str) -> str:
    """
    Utility function that will

    :param basename: The name of the executable without extension.

    :return: The executable with extension corresponding to the platform.
    """
    if os.name == "nt":
        return f"{basename}.exe"
    else:
        return basename


def shift_gcc_gutter_line_number(offset: int, line: str) -> str:
    """
    Shift the line number in the gutter of a GCC source snippet line, keeping the
    gutter's width.
    """
    match = gcc_gutter_regex.match(line)
    if not match:
        return line
    spaces, number, bar = match.groups()
    width = len(spaces) + len(number)
    return f"{int(number) + offset:>{width}}{bar}{line[match.end() :]}"


def _convert_stacktrace_to_html(stacktrace: str) -> ExtendedMessage:
    code_regex_escaped = r"&lt;code&gt;:(\d+)"
    link_replacement = r'<a href="#" class="tab-link" data-tab="code" data-line="\1">&lt;code&gt;:\1</a>'
    # Escape special characters.
    stacktrace = html.escape(stacktrace)
    # Add links to code.
    stacktrace = re.sub(code_regex_escaped, link_replacement, stacktrace)
    # We cannot generate a "pre" element, since that is ugly.
    generated = f"<div class='code'>{stacktrace.strip()}</div>"
    # Ensure newlines.
    generated = generated.replace("\n", "<br>")
    return ExtendedMessage(
        description=generated,
        format="html",
        permission=Permission.STUDENT,
    )


def _strip_tested_directory(config: DodonaConfig, stacktrace: str) -> str:
    """Make paths relative to the unit's folder, so TESTed's own folders stay hidden."""
    tested_directory = config.tested_directory
    directories = {str(tested_directory), str(tested_directory.resolve())}
    prefixes = "|".join(re.escape(directory) for directory in directories)
    return re.sub(rf"(?:{prefixes})/[^/\s]+/", "", stacktrace)


def _replace_code_line_number(offset: int, stacktrace: str) -> str:
    def modify_line_number(match: re.Match) -> str:
        current_number = int(match.group(1))
        return "<code>:" + str(current_number + offset)

    return re.sub(code_regex, modify_line_number, stacktrace)


@overload
def convert_stacktrace_to_clickable_feedback(
    lang: Language, stacktrace: str
) -> ExtendedMessage: ...


@overload
def convert_stacktrace_to_clickable_feedback(
    lang: Language, stacktrace: None
) -> None: ...


def convert_stacktrace_to_clickable_feedback(
    lang: Language, stacktrace: str | None
) -> ExtendedMessage | None:
    """
    Convert a stacktrace to an HTML message with clickable links to the submission.

    In this process, the stacktrace is first passed to the language config to clean
    up the stacktrace. Then the code links are updated for shifted line numbers, and
    finally, the code links are converted to actual links.

    :param lang: The language config.
    :param stacktrace: The stacktrace to clean up.
    :return: A clickable feedback message.
    """
    if not stacktrace:
        return None
    cleaned_stacktrace = lang.cleanup_stacktrace(stacktrace)
    assert lang.config
    cleaned_stacktrace = _strip_tested_directory(lang.config.dodona, cleaned_stacktrace)
    updated_stacktrace = _replace_code_line_number(
        lang.config.dodona.source_offset, cleaned_stacktrace
    )
    html_stacktrace = _convert_stacktrace_to_html(updated_stacktrace)
    return html_stacktrace


def convert_unknown_type(value: StringType) -> str:
    assert value.type == BasicStringTypes.UNKNOWN
    # This is only used to generate values for displaying, so it does not have
    # to be valid code.
    result = value.data
    if value.diagnostic and value.diagnostic not in result:
        result = f"({value.diagnostic}) {result}"
    return result


def is_special_void_call(input_: PreparedTestcaseStatement, lang: Language) -> bool:
    """
    Check if the given testcase is a call to a special function with a "void" return
    value.
    """
    return (
        isinstance(input_.statement, FunctionCall)
        and input_.value_function is not None
        and lang.is_void_method(input_.statement.name)
    )
