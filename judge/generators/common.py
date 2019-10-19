"""Code generators for the testplans."""
from dataclasses import dataclass
from typing import List

from tested import Config
from testplan import Plan, Context


@dataclass
class ExecutionResult:
    stdout: List[str]
    stderr: List[str]
    results: List[str]


class Generator:
    def __init__(self, config: Config):
        self.config = config

    def generate_code(self, plan: Plan) -> List[str]:
        """
        Generate the code necessary for execution. All code should be generated
        for the whole testplan.
        """
        raise NotImplementedError

    def needs_compilation(self):
        """Return True if there needs to be a compilation step."""
        return False

    def compile(self):
        """Do the compilation step."""
        if self.needs_compilation():
            raise NotImplementedError
        else:
            pass

    def execute(self, context_id: str, context: Context) -> ExecutionResult:
        """
        Args:
            context_id: The ID of the given context.
            context: The actual context.
        """
        raise NotImplementedError
