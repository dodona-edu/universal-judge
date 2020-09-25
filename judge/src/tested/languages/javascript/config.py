from pathlib import Path
from typing import List

from tested.languages.config import Command, Config, Language


class JavaScript(Language):

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return ['node', file, *arguments]
