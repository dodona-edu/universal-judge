from pathlib import Path
from typing import List

from tested.languages import Language
from tested.languages.config import CallbackResult, Command


class Java(Language):

    def compilation(self, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"
        others = [x for x in files if not x.endswith(".jar")]
        return ["javac", "-cp", ".", *others], file_filter

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        return ["java", "-cp", ".", Path(file).stem, *arguments]
