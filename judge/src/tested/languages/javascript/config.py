from pathlib import Path
from typing import List

from tested.languages import Language
from tested.languages.config import Command, Config


class JavaScript(Language):

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return ['node', file, *arguments]
