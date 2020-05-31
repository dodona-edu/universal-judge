from pathlib import Path
from typing import List

from tested.languages import Language
from tested.languages.config import CallbackResult, Command, Config


class ${name}(Language):

    % if compiled:
    def compilation(self, config: Config, files: List[str]) -> CallbackResult:
        raise NotImplementedError
    % endif

    def execution(self, config: Config, cwd: Path, file: str, arguments: List[str]) -> Command:
        raise NotImplementedError
