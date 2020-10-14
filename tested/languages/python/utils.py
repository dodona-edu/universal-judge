import logging
import re

logger = logging.getLogger(__name__)


# Idea and original code: dodona/judge-pythia
def cleanup_stacktrace(traceback: str, reduce_all=False) -> str:
    """
    Takes a traceback as a string or as a list of strings and returns a reduced
    version of the traceback as a list of strings.
    """
    context_file_regex = re.compile(r"context_[0-9]+_[0-9]+\.py")

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

        logger.debug(line)
        logger.debug(context_file_regex.search(line))
        logger.debug(skip_line and (not line.startswith(' ') or 'File' in line))
        # skip line if not a new File line is started
        if context_file_regex.search(line):
            logger.debug('>>> Skip')
            skip_line = True
            continue
        elif skip_line and (not line.startswith(' ') or 'File' in line):
            logger.debug('>>> No skip')
            skip_line = False
        elif skip_line:
            continue

        # replace references to local names
        if 'File "./submission.py"' in line:
            line = line.replace('File "./submission.py"', 'File "<code>"')
        elif 'File "<string>"' in line:
            line = line.replace('File "<string>"', 'File "<code>"')
        elif 'File "<doctest>"' in line:
            continue
        elif 'File "' in line:
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
