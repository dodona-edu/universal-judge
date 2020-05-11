from pathlib import Path
from typing import List

from tested.languages import Language
from tested.languages.config import executable_name, Command


class JavaScript(Language):

    def execution(self, cwd: Path, file: str, arguments: List[str]) -> Command:
        local_file = cwd / executable_name(Path(file).stem)
        return ['node', str(local_file.absolute()), *arguments]
