import re
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
        regex = re.compile(f'{self.execution_prefix()}_[0-9]+_[0-9]+\\.'
                           f'{self.extension_file()}: [a-zA-Z_]+ [0-9]+:')
        script = f'./{self.with_extension(self.submission_name(bundle.plan))}'
        stderr = regex.sub("<testcode>:", stderr).replace(script, '<code>')
        regex = re.compile(f'{self.execution_prefix()}_[0-9]+_[0-9]+\\.'
                           f'{self.extension_file()}')
        return [], [], regex.sub("<testcode>", stderr)

    def stdout(self,
               bundle: Bundle,
               stdout: str) -> Tuple[List[Message], List[AnnotateCode], str]:
        return self.stderr(bundle, stdout)

    def linter(self, bundle: Bundle, submission: Path, remaining: float) \
            -> Tuple[List[Message], List[AnnotateCode]]:
        # Import locally to prevent errors.
        from tested.languages.bash import linter
        return linter.run_shellcheck(bundle, submission, remaining)
