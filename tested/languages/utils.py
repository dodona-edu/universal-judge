import logging
import re
from pathlib import Path

from tested.configs import Bundle
from tested.languages.config import Config, Language


logger = logging.getLogger(__name__)


def cleanup_description(lang_config: Language,
                        namespace: str,
                        description: str) -> str:
    return description.replace(
        rf'{lang_config.conventionalize_namespace(namespace)}.',
        r'', 1)


def jvm_memory_limit(config: Config) -> int:
    """
    Get the memory limit in bytes. Java Virtual Machine (JVM) requires this to be a
    multiple of 1024.
    See https://docs.oracle.com/en/java/javase/14/docs/specs/man/java.html
    """
    limit = int(config.memory_limit)
    limit = (limit // 1024) * 1024
    return limit


# Idea and original code: dodona/judge-pythia
def jvm_cleanup_stacktrace(traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
    context_file_regex = re.compile(r"(Context[0-9]+|Selector)")
    unresolved_main_regex = r"error: unresolved reference: solutionMain"
    unresolved_reference_regex = \
        re.compile(r"(error: unresolved reference: [a-zA-Z$_0-9]+)")

    if isinstance(traceback, str):
        traceback = traceback.splitlines(True)

    skip_line, lines = False, []
    for line in traceback:

        line = line.strip('\n')

        if not line:
            continue

        # skip line if not a new File line is started
        if context_file_regex.search(line):
            if unresolved_main_regex in line:
                lines.append('error: unresolved reference: main\n')
            else:
                match = unresolved_reference_regex.search(line)
                if match:
                    lines.append(match[0] + '\n')
            skip_line = True
            continue
        elif skip_line and (line.startswith(' ') and 'at' not in line):
            continue

        # replace references to local names
        if submission_file in line:
            line = line.replace(submission_file, '<code>')
        elif 'at ' in line:
            skip_line = True
            continue
        skip_line = False

        if not (reduce_all and line.startswith(' ')):
            lines.append(line + '\n')

    if len(lines) > 20:
        lines = lines[:19] + ['...\n'] + [lines[-1]]
    return "".join(lines)


def haskell_solution(lang_config: Language, solution: Path, bundle: Bundle):
    """Support implicit modules if needed."""
    if bundle.config.config_for().get("implicitModule", True):
        name = lang_config.submission_name(bundle.plan)
        # noinspection PyTypeChecker
        with open(solution, "r") as file:
            contents = file.read()
        # noinspection PyTypeChecker
        with open(solution, "w") as file:
            result = f"module {name} where\n" + contents
            file.write(result)


def haskell_cleanup_stacktrace(traceback: str,
                               submission_file: str,
                               reduce_all=False):
    context_file_regex = re.compile(r"(Context[0-9]+|Selector)")
    called_at_regex = re.compile(r"^(.*called at ./<code>:)([0-9]+)(:[0-9]+) .*$")
    compile_line_regex = re.compile(r"^([0-9]+)(\s*\|.*)$")
    type_conflict_regex = re.compile(
        r"Couldn.t match expected type (.*) with actual type (.*)"
    )

    parse_module = r"error: parse error on input ‘module’"
    replace_module = r"error: unexpected ‘module’"

    if isinstance(traceback, str):
        traceback = traceback.splitlines(True)

    skip_line, lines = False, []
    for line in traceback:

        line = line.strip('\n')

        if not line or line == 'undefined':
            continue

        # skip line if not a new File line is started
        if context_file_regex.search(line):
            skip_line = True
            continue
        elif skip_line and (line.startswith(' ') or line[0].isdigit()):
            match = type_conflict_regex.search(line)
            if match:
                lines.append("Argument type conflict: Couldn't match expected type "
                             f"{match.group(2)} with actual type "
                             f"{match.group(1)}\n")
            continue

        # replace references to local names
        if submission_file in line:
            line = line.replace(submission_file, '<code>')
            match = called_at_regex.match(line)
            if match:
                line = f"{match.group(1)}{int(match.group(2)) - 1}{match.group(3)}"
        elif 'at ' in line:
            skip_line = True
            continue
        skip_line = False

        if parse_module in line:
            line = line.replace(parse_module, replace_module)
        else:
            match = compile_line_regex.match(line)
            if match:
                line = f"{int(match.group(1)) - 1}{match.group(2)}"

        if not (reduce_all and line.startswith(' ')):
            lines.append(line + '\n')

    if len(lines) > 20:
        lines = lines[:19] + ['...\n'] + [lines[-1]]
    return "".join(lines)
