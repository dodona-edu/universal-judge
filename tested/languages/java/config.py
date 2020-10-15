import logging
import re
from pathlib import Path
from typing import List

from tested.languages.config import CallbackResult, Command, Config, Language
from tested.languages.utils import jvm_memory_limit, jvm_cleanup_stacktrace

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

    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        return jvm_cleanup_stacktrace(traceback, submission_file, reduce_all)
