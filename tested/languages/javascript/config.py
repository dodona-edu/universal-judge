import logging
from esprima import parseScript, error_handler
from pathlib import Path
from typing import List

from tested.configs import Bundle
from tested.languages.config import Command, Config, Language

logger = logging.getLogger(__name__)


class JavaScript(Language):

    def execution(self, config: Config,
                  cwd: Path, file: str, arguments: List[str]) -> Command:
        return ['node', file, *arguments]

    # noinspection PyTypeChecker
    def solution(self, solution: Path, bundle: Bundle):
        try:
            with open(solution, "r") as file:
                contents = file.read()
            body = parseScript(contents).body
            functions = filter(
                lambda x: x.type in ('FunctionDeclaration', 'ClassDeclaration'),
                body
            )
            functions = filter(
                lambda x: x.id and x.id.type == 'Identifier',
                functions
            )
            functions = map(lambda x: x.id.name, functions)
            with open(solution, "a") as file:
                print("\nmodule.exports = {", ", ".join(functions), "};", file=file)
        except error_handler.Error:
            logger.debug("Failing to parse submission")
