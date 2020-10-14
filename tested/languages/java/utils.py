import re


# Idea and original code: dodona/judge-pythia
def cleanup_stacktrace(traceback: str, reduce_all=False) -> str:
    """
    Takes a traceback as a string or as a list of strings and returns a reduced
    version of the traceback as a list of strings.
    """
    context_file_regex = re.compile(r"Context[0-9]+|Selector")

    if isinstance(traceback, str):
        traceback = traceback.splitlines(True)

    skip_line, lines = False, []
    for line in traceback:

        line = line.strip('\n')

        if not line:
            continue

        if line.startswith('During handling of the above exception, another '
                           'exception occurred:'):
            lines = []
            continue

        # skip line if not a new File line is started
        if context_file_regex.search(line):
            skip_line = True
            continue
        elif skip_line and (not line.startswith(' ') or 'at' in line):
            skip_line = False
        elif skip_line:
            continue

        # replace references to local names
        if 'Submission.java' in line:
            line = line.replace('Submission.java', '<code>')
        elif 'at ' in line:
            skip_line = True
            continue
        skip_line = False

        # replace references to modules
        if ', in <module>' in line:
            line = line.replace(', in <module>', '')

        if not (reduce_all and line.startswith(' ')):
            lines.append(line + '\n')

    if len(lines) > 20:
        lines = lines[:19] + ['...\n'] + [lines[-1]]
    return "".join(lines)
