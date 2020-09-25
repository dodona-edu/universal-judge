from pathlib import Path
from typing import List

from tested.languages.config import CallbackResult, Command, Config, Language

def _memory_limit(config: Config) -> int:
    """
    Get the memory limit in bytes. Java requires this to be a multiple of 1024.
    See https://docs.oracle.com/en/java/javase/14/docs/specs/man/java.html
    """
    limit = int(config.memory_limit)
    limit = (limit // 1024) * 1024
    return limit


class Java(Language):

    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        def file_filter(file: Path) -> bool:
            return file.suffix == ".class"
        others = [x for x in files if not x.endswith(".jar")]
        return ["javac", "-cp", ".", *others], file_filter

    def execution(self, config: Config, 
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        limit = _memory_limit(config)
        return ["java", f"-Xmx{limit}", "-cp", ".", Path(file).stem, *arguments]
