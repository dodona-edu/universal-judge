import re

from tested.languages.config import Config


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
    unresolved_main_regex = re.compile(r"error: unresolved reference: solutionMain")
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
            if unresolved_main_regex.search(line):
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
