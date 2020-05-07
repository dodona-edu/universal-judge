from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages import Language
from tested.languages.config import CallbackResult, executable_name, Command


class ${name}(Language):

    % if compiled:
    def compilation(self, files: List[str]) -> CallbackResult:
        raise NotImplementedError
    % endif
    
    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        raise NotImplementedError
