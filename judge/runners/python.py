import random
import string
import subprocess
from typing import List, Tuple

import jinja2

from dodona.common import Message, ExtendedMessage, Permission
from runners.common import Runner, ExecutionResult
from tested import Config
from testplan import Context, Plan, Tab, ChannelState, Testcase, DataType, TestPlanError, _get_input, FunctionCall, \
    FunctionType, Type


def _get_identifier() -> str:
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


class PythonRunner(Runner):

    def main(self, c: FunctionCall) -> str:
        return ""

    def __init__(self, config: Config):
        super().__init__(config)
        self.identifier = _get_identifier()

    def generate_code(self, submission: str, plan: Plan) -> List[str]:
        loader = jinja2.FileSystemLoader(searchpath=f"{self.config.judge}/judge/runners/templates/python")
        environment = jinja2.Environment(loader=loader, undefined=jinja2.StrictUndefined)

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
                    Type=Type
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
            stdin_.append("\n".join(_get_input(testcase)))
        stdin_ = "\n".join(stdin_)

        # First, we start the code.
        p = subprocess.run(['python', file], input=stdin_,
                           timeout=timeout, text=True, capture_output=True)
        identifier = f"--{self.identifier}--"
        start = f"{identifier} START"
        separator = f"{identifier} SEP"
        end = f"{identifier} END"
        # Check that the code has started
        messages = []
        # problem = False
        # if not p.stdout.startswith(start):
        #     messages.append(ExtendedMessage("stdout did not start with proper begin tag", "text", Permission.STAFF))
        #     messages.append(ExtendedMessage("A problem occurred before evaluating your code", "text", Permission.STUDENT))
        #     problem = True
        # if not p.stderr.startswith(start):
        #     messages.append(ExtendedMessage("stderr did not start with proper begin tag", "text", Permission.STAFF))
        #     messages.append(ExtendedMessage("A problem occurred before evaluating your code", "text", Permission.STUDENT))
        #     problem = True
        #
        # if not p.stdout.endswith(end):
        #     messages.append(ExtendedMessage("stdout did not end with proper end tag", "text", Permission.STAFF))
        #     messages.append(ExtendedMessage("Could not complete the evaluation of your code", "text", Permission.STUDENT))
        #     problem = True
        # if not p.stderr.endswith(end):
        #     messages.append(ExtendedMessage("stderr did not end with proper end tag", "text", Permission.STAFF))
        #     messages.append(ExtendedMessage("Could not complete the evaluation of your code", "text", Permission.STUDENT))
        #     problem = True

        stdout_ = p.stdout.split(separator)
        stderr_ = p.stderr.split(separator)

        return ExecutionResult(stdout_, stderr_, [], p.returncode)
