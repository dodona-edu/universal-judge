import logging
from pathlib import Path
from typing import List, Tuple

from tested.configs import Bundle
from tested.dodona import Message, AnnotateCode
from tested.languages.config import CallbackResult, Command, Config, Language, \
    limit_output
from tested.languages.utils import jvm_memory_limit, jvm_cleanup_stacktrace, \
    jvm_stderr

logger = logging.getLogger(__name__)


class Java(Language):

    def compilation(self, bundle: Bundle, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"

        others = [x for x in files if not x.endswith(".jar")]
        return ["javac", "-cp", ".", *others], file_filter

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        limit = jvm_memory_limit(config)
        return ["java", f"-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]

    def linter(self, bundle: Bundle, submission: Path, remaining: float) \
            -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.java import linter
        return linter.run_checkstyle(bundle, submission, remaining)

    def cleanup_stacktrace(self,
                           traceback: str,
                           submission_file: str,
                           reduce_all=False) -> str:
        return jvm_cleanup_stacktrace(traceback, submission_file, reduce_all)

    def compiler_output(
            self, namespace: str, stdout: str, stderr: str
    ) -> Tuple[List[Message], List[AnnotateCode], str, str]:
        return [], [], limit_output(stdout), jvm_cleanup_stacktrace(
            stderr,
            self.with_extension(self.conventionalize_namespace(namespace))
        )

    def stderr(self,
               bundle: Bundle,
               stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        return jvm_stderr(self, bundle, stderr)
