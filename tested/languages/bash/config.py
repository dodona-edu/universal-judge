from pathlib import Path
from typing import List, Tuple

from tested.configs import Bundle
from tested.dodona import Message, AnnotateCode
from tested.languages.config import CallbackResult, Command, Config, Language


class Bash(Language):

    def execution(self, config: Config, cwd: Path, file: str,
                  arguments: List[str]) -> Command:
        return ['bash', file, *arguments]

    def stderr(self,
               bundle: Bundle,
               stderr: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        script = f'./{self.with_extension(self.submission_name(bundle.plan))}'
        return [], [], stderr.replace(script, '<code>')
