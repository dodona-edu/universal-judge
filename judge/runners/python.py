import random
import string
import subprocess
from typing import List, Tuple

import jinja2

from runners.common import Runner, ExecutionResult
from tested import Config
from testplan import Context, Plan, Tab, ChannelState, Testcase, DataType, TestPlanError, _get_input


def _get_identifier() -> str:
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


class PythonRunner(Runner):

    def __init__(self, config: Config):
        super().__init__(config)
        self.identifier = _get_identifier()

    def generate_code(self, submission: str, plan: Plan) -> List[str]:
        print("Generating code for plan...")
        loader = jinja2.FileSystemLoader(searchpath=f"{self.config.judge}/runners/templates/python")
        environment = jinja2.Environment(loader=loader)

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
                context_result = context_template.render(
                    code_identifier=self.identifier,
                    output_file=f"{self.config.workdir}/output.txt",
                    postprocessing=[]
                )
                with open(self.config.workdir + f"/context-{id_}.py", "w") as file:
                    file.write(context_result)
                context_ids.append(id_)
        return context_ids

    def execute(self, context_id: str, context: Context, timeout=None) -> ExecutionResult:
        print(f"Executing context {context_id}")
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
        stdout_ = p.stdout.strip(identifier).split(identifier)
        stderr_ = p.stderr.strip(identifier).split(identifier)

        return ExecutionResult(stdout_, stderr_, [], p.returncode)
