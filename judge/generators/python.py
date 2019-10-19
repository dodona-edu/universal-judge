from subprocess import Popen, PIPE

import jinja2

from generators.common import Generator
from testplan import Context, Plan, Tab


class PythonGenerator(Generator):

    def generate_code(self, plan: Plan):
        loader = jinja2.FileSystemLoader(searchpath="./templates/python")
        environment = jinja2.Environment(loader=loader)

        # Generate the submission code
        # TODO: just copy this file?
        with open(self.config.source, 'r') as file:
            submission_code = file.read()

        submission_template = environment.get_template("submission.jinja2")
        submission_result = submission_template.render(code=submission_code)
        with open(self.config.workdir + "/submission.py", "w") as file:
            file.write(submission_result)

        # We generate a new file for each context.
        context_ids = []
        for tab_idx, tab in plan.tabs:
            for context_idx, context in tab.contexts:
                id_ = f"{tab_idx}-{context_idx}"
                context_template = environment.get_template("context.jinja2")
                context_result = context_template.render(
                    code_identifier="RANDOM",
                    output_file=f"{self.config.workdir}/output.txt",
                    postprocessing=[]
                )
                with open(self.config.workdir + f"/context-{id_}.py", "w") as file:
                    file.write(context_result)
                context_ids.append(id_)
        return context_ids

    def execute(self, context_id: str, context: Context):
        file = self.config.workdir + f"/context-{context_id}.py"
        p = Popen(['python', self], stdout=PIPE, stderr=PIPE, stdin=)
        json_string = p.stdout.read()
            pass
