from pathlib import Path
from typing import List

% if compiled:
from tested.configs import Bundle
% endif
from tested.languages.config import CallbackResult, Command, Config, Language


class ${name}(Language):

    % if compiled:
    def compilation(self, bundle: Bundle, files: List[str]) -> CallbackResult:
        raise NotImplementedError
    % endif

    def execution(self, config: Config, cwd: Path, file: str, arguments: List[str]) -> Command:
        raise NotImplementedError
