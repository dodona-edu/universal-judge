from pathlib import Path
from typing import List

from tested.languages import Language
from tested.languages.config import CallbackResult, Command


class JavaConfig(Language):

    def c_compilation(self, files: List[str]) -> CallbackResult:
        def file_filter(file: Path, context: str) -> bool:
            is_class = file.suffix == ".class"
            is_context = file.name.startswith("Context")
            is_our_context = file.name.startswith(context + ".")
            return is_class and (not is_context or is_our_context)
        others = [x for x in files if not x.endswith(".jar")]
        return ["javac", "-cp", ".", *others], file_filter

    def c_execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        return ["java", "-cp", ".", Path(file).stem, *arguments]
