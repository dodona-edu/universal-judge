"""Code generators for the testplans."""
from dataclasses import dataclass
from typing import List

from tested import Config
from testplan import Plan, Context


@dataclass
class ExecutionResult:
    """
    The result of an execution.

    All output streams are divided per testcase, in the same order as the context that was used to
    execute the test. E.g. the string at position 0 in stdout is the result of executing the
    testcase at position 0 in the context.
    """
    stdout: List[str]
    stderr: List[str]
    results: List[str]
    value: int  # The return value.


class Runner:
    def __init__(self, config: Config):
        self.config = config

    def generate_code(self, submission: str, plan: Plan) -> List[str]:
        """
        Generate the code necessary for execution. All code should be generated
        for the whole testplan.
        """
        raise NotImplementedError

    # noinspection PyMethodMayBeStatic
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
