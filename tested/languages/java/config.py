import logging
import re
from pathlib import Path
from typing import List

from tested.languages.config import CallbackResult, Command, Config, Language
from tested.languages.utils import jvm_memory_limit

logger = logging.getLogger(__name__)


class Java(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return ["javac", "-cp", ".", *others], file_filter

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        limit = jvm_memory_limit(config)
        return ["java", f"-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]

    # Idea and original code: dodona/judge-pythia
    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        context_file_regex = re.compile(r"Context[0-9]+|Selector")

        if isinstance(traceback, str):
            traceback = traceback.splitlines(True)

        skip_line, lines = False, []
        for line in traceback:

            line = line.strip('\n')

            if not line:
                continue

            # skip line if not a new File line is started
            if context_file_regex.search(line):
                skip_line = True
                continue
            elif skip_line:
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
