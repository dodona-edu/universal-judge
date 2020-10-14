import logging
from pathlib import Path
from typing import List

from tested.languages.config import CallbackResult, Command, Config, Language
from tested.languages.java.utils import cleanup_stacktrace
from tested.languages.utils import jvm_memory_limit
from tested.serialisation import ExceptionValue


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

    def exception_output(self, exception: ExceptionValue) -> ExceptionValue:
        exception.stacktrace = cleanup_stacktrace(exception.stacktrace)
        return exception
