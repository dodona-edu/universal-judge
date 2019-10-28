"""Code generators for the testplans."""
from dataclasses import dataclass
from pathlib import Path
from typing import List

import jinja2

from tested import Config
from testplan import Plan, Context, FunctionArg, ValueType, FunctionCall, FunctionType, TestPlanError


@dataclass
class ExecutionResult:
    """
    The result of an execution.

    All output streams are divided per testcase, in the same order as the context that was used to
    execute the test. E.g. the string at position 0 in stdout is the result of executing the
    testcase at position 0 in the context.
    """
    separator: str
    stdout: str
    stderr: str
    results: str
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

    def needs_main(self):
        """Return True if the language requires a main function."""
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

    def _path_to_templates(self) -> Path:
        return Path(self.config.judge, 'judge', 'runners', 'templates', self.config.programming_language)

    def _get_environment(self) -> jinja2.Environment:
        """Get the environment for the templates."""
        loader = jinja2.FileSystemLoader(str(self._path_to_templates()))
        return jinja2.Environment(loader=loader, undefined=jinja2.StrictUndefined)

    # def argument_template(self, argument: FunctionArg) -> str:
    #     """Produce code for a single argument"""
    #     env = self.__get_environment()
    #     template = env.get_template("argument.jinja2")
    #     return template.render(
    #         argument=argument,
    #         ValueType=ValueType
    #     )

    def function_call(self, call: FunctionCall) -> str:
        """Produce code for a single function call"""
        env = self._get_environment()
        template = env.get_template("function.jinja2")
        return template.render(
            function=call,
            FunctionType=FunctionType,
            FunctionCall=FunctionCall,
            ValueType=ValueType
        )

    def execution_args(self, c: Context) -> dict:

        if c.execution.input.function.type != FunctionType.main:
            raise TestPlanError("Main function must have type main")

        return {
            "args": c.execution.input.function.arguments,
            "call": self.function_call(c.execution.input.function)
        }
