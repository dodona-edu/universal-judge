import html
import logging
import math
import os
import re
from pathlib import Path
from typing import TYPE_CHECKING, List, Tuple

from tested.configs import GlobalConfig
from tested.dodona import AnnotateCode, ExtendedMessage, Message, Permission
from tested.languages.conventionalize import submission_file, submission_name

if TYPE_CHECKING:
    from tested.languages.config import Language

_logger = logging.getLogger(__name__)


def cleanup_description(lang_config: "Language", description: str) -> str:
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
def jvm_cleanup_stacktrace(traceback: str, submission_filename: str) -> str:
    context_file_regex = re.compile(r"(Context[0-9]+|Selector)")
    unresolved_main_regex = r"error: unresolved reference: solutionMain"
    unresolved_reference_regex = re.compile(
        r"(error: unresolved reference: [a-zA-Z$_0-9]+)"
    )

    if isinstance(traceback, str):
        traceback = traceback.splitlines(True)

    skip_line, lines = False, []
    for line in traceback:
        line = line.strip("\n")

        if not line:
            continue

        # skip line if not a new File line is started
        if context_file_regex.search(line):
            if unresolved_main_regex in line:
                lines.append("error: unresolved reference: main\n")
            else:
                match = unresolved_reference_regex.search(line)
                if match:
                    lines.append(match[0] + "\n")
            skip_line = True
            continue
        elif skip_line and (line.startswith(" ") and "at" not in line):
            continue

        # replace references to local names
        if submission_filename in line:
            line = line.replace(submission_filename, "<code>")
            line = line.replace("Kt.solutionMain", "Kt.main")
        elif "at " in line:
            skip_line = True
            continue
        skip_line = False
        lines.append(line + "\n")

    if len(lines) > 20:
        lines = lines[:19] + ["...\n"] + [lines[-1]]
    return "".join(lines)


def jvm_stderr(
    self: "Language", stderr: str
) -> Tuple[List[Message], List[AnnotateCode], str]:
    # Identifier to separate testcase output
    identifier = f"--{self.config.testcase_separator_secret}-- SEP"
    context_identifier = f"--{self.config.context_separator_secret}-- SEP"
    submission = submission_file(self)

    return (
        [],
        [],
        context_identifier.join(
            identifier.join(
                self.cleanup_stacktrace(testcase)
                for testcase in context.split(identifier)
            )
            for context in stderr.split(context_identifier)
        ),
    )


def haskell_solution(lang_config: "Language", solution: Path):
    """Support implicit modules if needed."""
    if lang_config.config.dodona.config_for().get("implicitModule", True):
        name = submission_name(lang_config)
        # noinspection PyTypeChecker
        with open(solution, "r") as file:
            contents = file.read()
        # noinspection PyTypeChecker
        with open(solution, "w") as file:
            original_lines = contents.split("\n")
            i = next(
                i for i, line in enumerate(original_lines) if not line.startswith("{-#")
            )
            result = f"module {name} where\n"
            resulting_lines = original_lines[:i] + [result] + original_lines[i:]
            file.write("\n".join(resulting_lines))


def haskell_cleanup_stacktrace(traceback: str, submission_filename: str):
    context_file_regex = re.compile(r"(Context[0-9]+|Selector)")
    called_at_regex = re.compile(r"^(.*called at )./(<code>:)([0-9]+)(:[0-9]+) .*$")
    compile_line_regex = re.compile(r"^([0-9]+)(\s*\|.*)$")
    type_conflict_regex = re.compile(
        r"Couldn.t match expected type (.*) with actual type (.*)"
    )
    code_line_regex = re.compile(r"^(.*<code>:)([0-9]+)(:[0-9]+.*)$")

    parse_module = r"error: parse error on input ‘module’"
    replace_module = r"error: unexpected ‘module’"

    if isinstance(traceback, str):
        traceback = traceback.splitlines(True)

    skip_line, lines = False, []
    for line in traceback:
        line = line.strip("\n")

        if not line or line == "undefined":
            continue

        # skip line if not a new File line is started
        if context_file_regex.search(line):
            skip_line = True
            continue
        elif skip_line and (line.startswith(" ") or line[0].isdigit()):
            match = type_conflict_regex.search(line)
            if match:
                lines.append(
                    "Argument type conflict: Couldn't match expected type "
                    f"{match.group(2)} with actual type "
                    f"{match.group(1)}\n"
                )
            continue

        # replace references to local names
        if submission_filename in line:
            line = line.replace(submission_filename, "<code>")
            match = called_at_regex.match(line)
            if match:
                line = (
                    f"{match.group(1)}{match.group(2)}"
                    f"{int(match.group(3)) - 1}{match.group(4)}"
                )
            else:
                match = code_line_regex.match(line)
                if match:
                    line = (
                        f"{match.group(1)}{int(match.group(2)) - 1}" f"{match.group(3)}"
                    )
        elif "at " in line:
            skip_line = True
            continue
        skip_line = False

        if parse_module in line:
            line = line.replace(parse_module, replace_module)
        else:
            match = compile_line_regex.match(line)
            if match:
                line = f"{int(match.group(1)) - 1}{match.group(2)}"

        lines.append(line + "\n")

    if len(lines) > 20:
        lines = lines[:19] + ["...\n"] + [lines[-1]]
    return "".join(lines)


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


def limit_output(
    output: str,
    limit_characters: int = 512,
    max_lines: int = 20,
    ellipsis_str: str = "...",
) -> str:
    """
    Utility function for limiting a string output

    :param output: String that possible needs to be abbreviated
    :param limit_characters: Maximum characters used in the output
    :param max_lines: Maximum lines in the output
    :param ellipsis_str: ellipsis used when abbreviated is needed

    :return: The abbreviated 'output' if needed otherwise the 'output' itself
    """
    lines = output.splitlines()
    # Case character limit not exceeded and line limit not exceeded
    if len(output) <= limit_characters and len(lines) <= max_lines:
        return output
    # Case character limit exceeded
    max_chars = limit_characters - len(ellipsis_str)
    forward_buffer = []
    backward_buffer = []
    len_lines = len(lines)
    for f in range(math.ceil(min(max_lines - 1, len_lines) / 2)):
        r = len_lines - f - 1
        # Case last lines to consider are the same
        if f == r:
            forward_buffer.append(lines[f][: (max_chars - 1)])
        # Otherwise
        else:
            next_line, prev_line = lines[f], lines[r]
            current_length = len(next_line) + len(prev_line) + 2
            # Both lines can be add in full
            if current_length < max_chars:
                forward_buffer.append(next_line)
                backward_buffer.append(prev_line)
                max_chars -= current_length
            # Lines must be limited
            else:
                half = max_chars / 2
                # Next line can be add in full
                if len(next_line) + 2 < max_chars:
                    forward_buffer.append(next_line)
                    max_chars -= len(next_line) + 2
                    backward_buffer.append(prev_line[-max_chars:])
                # Prev line can be add in full
                elif len(prev_line) + 2 < max_chars:
                    backward_buffer.append(prev_line)
                    max_chars -= len(prev_line) + 2
                    forward_buffer.append(next_line[:max_chars])
                # Both lines needed abbreviation
                else:
                    forward_buffer.append(next_line[: math.ceil(half - 1)])
                    backward_buffer.append(prev_line[-math.floor(half - 1) :])
                # Terminate loop because character limit reached
                break
    # Concat buffer
    return "\n".join(forward_buffer + [ellipsis_str] + backward_buffer[::-1])


def trace_to_html(
    traceback: str,
    link_regex: str = r"&lt;code&gt;:([0-9]+)",
    link_subs: str = r'<a href="#" class="tab-link" data-tab="code" '
    r'data-line="\1">&lt;code&gt;:\1</a>',
) -> ExtendedMessage:
    # Escape special characters
    traceback = html.escape(traceback)
    # Compile regex
    link_regex = re.compile(link_regex)
    # Add links to
    traceback = link_regex.sub(link_subs, traceback)
    _logger.debug(f"<pre><code>{traceback}</code></pre>")
    return ExtendedMessage(
        description=f"<pre><code>{traceback}</code></pre>",
        format="html",
        permission=Permission.STUDENT,
    )
