"""Code generators for the testplans."""
import os
import random
import shutil
import string
import subprocess
from dataclasses import dataclass
from glob import glob
from pathlib import Path
from typing import List

import jinja2
from jinja2 import TemplateNotFound

from runners.config import LanguageConfig
from runners.java import JavaConfig
from runners.python import PythonConfig
from tested import Config
from testplan import _get_stdin, Context, FunctionCall, FunctionType, Plan, TestPlanError, ValueType


def _get_identifier() -> str:
    """Generate a random identifier valid in most languages."""
    letter = random.choice(string.ascii_letters)
    rest = random.sample(string.ascii_letters + string.digits, 8)
    return letter + ''.join(rest)


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


class BaseRunner:
    """
    Base runner to test a submission in a given programming language.

    For most language, it is enough to define a :class:`LanguageConfig`, and use the pre-made
    :class:`ConfigurableRunner`.
    """
    def __init__(self, config: Config):
        self.config = config

    def generate_code(self, submission: str, plan: Plan) -> List[str]:
        """
        Generate the code necessary for execution. All code should be generated for the whole
        testplan.

        Each context in the test plan is given an unique ID, which can be used to retrieve the
        results of the execution later.

        :param submission: The code that must be tested.
        :param plan: The test plan to execute for the given submission.
        :return: A list of context ids.
        """
        raise NotImplementedError

    def execute(self, context_id: str, context: Context, timeout=None) -> ExecutionResult:
        """
        Execute the tests for a given context.

        :param timeout: Time-out for the evaluation of the context.
        :param context_id: The ID of the context. This is an element returned in the list by the
        generation function.
        :param context: The actual context instance.
        :return: The result of the execution, which allows judging of the results.
        """
        raise NotImplementedError

    def compile(self) -> ExecutionResult:
        """Will be called if `needs_compilation` is True."""
        raise NotImplementedError

    def needs_compilation(self) -> bool:
        """True if the language needs an implementation step."""
        raise NotImplementedError

    def function_call(self, call: FunctionCall) -> str:
        """Create a textual representation of a function call, to display to the user."""
        raise NotImplementedError

    def needs_main(self):
        """True if the language needs a main function call to run."""
        raise NotImplementedError


class ConfigurableRunner(BaseRunner):
    def __init__(self, config: Config, language_config: LanguageConfig):
        super().__init__(config)
        self.language_config = language_config
        self.identifier = _get_identifier()

    def generate_code(self, submission: str, plan: Plan) -> List[str]:
        environment = self._get_environment()

        # Generate a file containing the submitted code.
        submission_name = f"{self.language_config.submission_name(plan)}.{self.language_config.file_extension()}"
        submission_template = self._find_template("submission", environment)
        submission_result = submission_template.render(code=submission)
        with open(Path(self.config.workdir, submission_name), "w") as file:
            file.write(submission_result)

        # Each context gets its own file.
        # We generate a new file for each context.
        context_ids = []
        for tab_idx, tab in enumerate(plan.tabs):
            for context_idx, context in enumerate(tab.contexts):
                id_ = f"{tab_idx}_{context_idx}"
                context_template = self._find_template("context", environment)
                # Variables for the main test case
                execution = self.get_execution(context)
                context_result = context_template.render(
                    execution=execution,
                    code_identifier=self.identifier,
                    output_file=str(Path(self.config.workdir, 'output.txt')).replace("\\", "/"),
                    additionals=context.additional,
                    FunctionType=FunctionType,
                    ValueType=ValueType,
                    context_id=id_,
                    has_top_level=self.language_config.supports_top_level_functions()
                )
                with open(Path(self.config.workdir, self._context_name(id_)), "w") as file:
                    file.write(context_result)
                context_ids.append(id_)

                # Copy additional files we might need.
        for file in self.language_config.additional_files():
            result = self._path_to_templates() / file
            # noinspection PyTypeChecker
            shutil.copy2(result, self.config.workdir)

        return context_ids

    def compile(self) -> ExecutionResult:
        file_argument = [os.path.basename(x) for x in glob(f"{self.config.workdir}/*.{self.language_config.file_extension()}")]
        command = self.language_config.compilation_command(file_argument)
        p = subprocess.run(command, text=True, capture_output=True, cwd=self.config.workdir)
        return ExecutionResult("", p.stdout, p.stderr, "", p.returncode)

    def _context_name(self, context_id: str) -> str:
        return f"{self.language_config.context_name(context_id)}.{self.language_config.file_extension()}"

    def needs_compilation(self) -> bool:
        return self.language_config.needs_compilation()

    def execute(self, context_id: str, context: Context, timeout=None) -> ExecutionResult:

        stdin_ = []
        for testcase in context.all_testcases():
            stdin_.append(_get_stdin(testcase))
        stdin_ = "\n".join(stdin_)

        command = self.language_config.execution_command(context_id, Path(self.config.workdir))
        p = subprocess.run(command, input=stdin_, timeout=timeout, text=True, capture_output=True)
        identifier = f"--{self.identifier}-- SEP"

        try:
            with open(f"{self.config.workdir}/output.txt", "r") as f:
                values = f.read()
        except FileNotFoundError:
            values = ""

        return ExecutionResult(identifier, p.stdout, p.stderr, values, p.returncode)

    def _path_to_templates(self) -> Path:
        """The path to the templates and additional files."""
        return Path(self.config.judge) / 'judge' / 'runners' / 'templates' / self.config.programming_language

    def _get_environment(self) -> jinja2.Environment:
        """Get the environment for the templates."""
        loader = jinja2.FileSystemLoader(str(self._path_to_templates()))
        return jinja2.Environment(loader=loader, undefined=jinja2.StrictUndefined)

    # noinspection PyMethodMayBeStatic
    def get_execution(self, c: Context) -> FunctionCall:
        if c.execution.input.function.type != FunctionType.main:
            raise TestPlanError("Main function must have type main")

        return c.execution.input.function

    def _find_template(self, name, environment):
        """Find a template with a name."""
        try:
            return environment.get_template(f"{name}.{self.language_config.file_extension()}")
        except TemplateNotFound:
            return environment.get_template(f"{name}.jinja2")

    def function_call(self, call: FunctionCall) -> str:
        """Produce code for a single function call."""
        env = self._get_environment()
        template = env.get_template("function.jinja2")
        return template.render(
            function=call,
            FunctionType=FunctionType,
            FunctionCall=FunctionCall,
            ValueType=ValueType,
            has_top_level=self.language_config.supports_top_level_functions()
        )

    def needs_main(self):
        return self.language_config.needs_main()


CONFIGS = {
    'python': PythonConfig,
    'java': JavaConfig
}


def get_runner(config: Config) -> BaseRunner:
    """Get the runner for the specified language."""
    return ConfigurableRunner(config, CONFIGS[config.programming_language]())
