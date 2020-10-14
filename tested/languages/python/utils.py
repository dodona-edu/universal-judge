import logging
import re

logger = logging.getLogger(__name__)


# Idea and original code: dodona/judge-pythia
def add_links_to_stacktrace(traceback: str) -> str:
    # insert links to source code lines
    new_content = []
    for line in traceback.splitlines():

        # find links to source code lines
        # NOTE: on Dodona, just (<code> or <doctest>) should be used
        #       instead of (source.py|...)
        match = re.search(r'File "(<code>|<doctest>)", line ([0-9]+)[,.*]?', line)
        logger.debug(match)
        # mark links to source code lines
        if match:
            replace = r'File \[\1, line \2\](#){: .tab-link data-tab="code" ' \
                      r'data-link="\2"}'
            line = re.sub(r'File "(<code>|<doctest>)", line ([0-9]+)[,.*]?',
                          replace,
                          line)
            new_content.append(line)
        else:
            new_content.append(line)

    # reconstruct output with links to source code lines
    return '\n'.join(new_content)
