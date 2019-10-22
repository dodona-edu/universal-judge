import random
import string
import subprocess
from typing import List

import jinja2

from runners.common import ExecutionResult, Runner
from tested import Config
from testplan import _get_stdin, Context, FunctionCall, FunctionType, Plan, ValueType


def _get_identifier() -> str:
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


class PythonRunner(Runner):

    def __get_environment(self) -> jinja2.Environment:
        loader = jinja2.FileSystemLoader(searchpath=f"{self.config.judge}/judge/runners/templates/python")
        return jinja2.Environment(loader=loader, undefined=jinja2.StrictUndefined)

    def function_call(self, call: FunctionCall) -> str:
        env = self.__get_environment()
        template = env.get_template("function.jinja2")
        return template.render(
            function=call,
            FunctionType=FunctionType,
            FunctionCall=FunctionCall,
            ValueType=ValueType
        )

    def __init__(self, config: Config):
        super().__init__(config)
        self.identifier = _get_identifier()

    def generate_code(self, submission: str, plan: Plan) -> List[str]:
        environment = self.__get_environment()

        submission_template = environment.get_template("submission.jinja2")
        submission_result = submission_template.render(code=submission)
        with open(f"{self.config.workdir}/submission.py", "w") as file:
            file.write(submission_result)

        # We generate a new file for each context.
        context_ids = []
        for tab_idx, tab in enumerate(plan.tabs):
            for context_idx, context in enumerate(tab.contexts):
                id_ = f"{tab_idx}-{context_idx}"
                context_template = environment.get_template("context.jinja2")
                # Variables for the main test case
                execution = self.execution_args(context)
                context_result = context_template.render(
                    execution=execution,
                    code_identifier=self.identifier,
                    output_file=f"{self.config.workdir}/output.txt",
                    additionals=context.additional,
                    FunctionType=FunctionType,
                    ValueType=ValueType
                )
                with open(self.config.workdir + f"/context-{id_}.py", "w") as file:
                    file.write(context_result)
                context_ids.append(id_)
        return context_ids

    def execute(self, context_id: str, context: Context, timeout=None) -> ExecutionResult:
        file = self.config.workdir + f"/context-{context_id}.py"

        # Collect stdin
        stdin_ = []
        for testcase in context.all_testcases():
            stdin_.append("\n".join(_get_stdin(testcase)))
        stdin_ = "\n".join(stdin_)

        # First, we start the code.
        p = subprocess.run(['python', file], input=stdin_,
                           timeout=timeout, text=True, capture_output=True)
        identifier = f"--{self.identifier}-- SEP"
        stdout_ = p.stdout
        stderr_ = p.stderr
        with open(f"{self.config.workdir}/output.txt", "r") as f:
            values = f.read()

        return ExecutionResult(identifier, stdout_, stderr_, values, p.returncode)
