"""Code generators for the testplans."""
from dataclasses import dataclass
from typing import List

from tested import Config
from testplan import Plan, Context, FunctionArg, Type, FunctionCall, FunctionType, TestPlanError
from dodona.common import ExtendedMessage


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

    def main(self, c: FunctionCall) -> str:
        raise NotImplementedError

    def execution_args(self, c: Context) -> dict:

        def convert_value(arg: FunctionArg) -> str:
            if arg.type == Type.text:
                return f'"{arg.data}"'
            else:
                return str(arg.data)

        input_ = c.execution.input
        args = input_.function.arguments
        converted = []
        for argument in args:
            if argument.name:
                converted.append(argument.name)
            converted.append(convert_value(argument))

        if input_.function.type != FunctionType.main:
            raise TestPlanError("Main function must have type main")

        call = self.main(c.execution.input.function)

        return {
            "args": converted,
            "call": call
        }
