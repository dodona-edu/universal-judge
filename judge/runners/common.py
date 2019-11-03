"""Code generators for the testplans."""
import random
import shutil
import string
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import List, Tuple

import jinja2
from jinja2 import TemplateNotFound

from runners.config import LanguageConfig
from runners.haskell import HaskellConfig
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

    def generate_code(self, submission: str, plan: Plan) -> Tuple[List[str], List[str]]:
        """
        Generate the code necessary for execution. All code should be generated for the whole
        testplan.

        Each context in the test plan is given an unique ID, which can be used to retrieve the
        results of the execution later.

        :param submission: The code that must be tested.
        :param plan: The test plan to execute for the given submission.
        :return: A list of context ids and an ordered list of files.
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

    def compile(self, ordered_files) -> ExecutionResult:
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

    def generate_code(self, submission: str, plan: Plan) -> Tuple[List[str], List[str]]:
        environment = self._get_environment()

        ordered_files = []

        # Copy additional files we might need.
        for file in self.language_config.additional_files():
            result = self._path_to_templates() / file
            # noinspection PyTypeChecker
            shutil.copy2(result, self.config.workdir)
        ordered_files.extend(self.language_config.additional_files())

        # Each context gets two files: the file containing the user code, and the file containing
        # the actual tests we run on the user code.
        # Language that have the same user code for all contexts can return the same name each time,
        # which causes the files to be overridden. This will be faster if compilation is needed.
        # TODO: In the future, we might not generate different files if the name is the same.
        context_ids = []
        for tab_idx, tab in enumerate(plan.tabs):
            for context_idx, context in enumerate(tab.contexts):
                id_ = f"{tab_idx}_{context_idx}"
                context_template = self._find_template("context", environment)

                # Create the submission file.
                submission_name = self.language_config.submission_name(id_, context)
                submission_template = self._find_template("submission", environment)
                submission_result = submission_template.render(
                    code=submission,
                    name=submission_name,
                    before=context.before.get(self.config.programming_language),
                    after=context.after.get(self.config.programming_language)
                )
                submission_file = f"{submission_name}.{self.language_config.file_extension()}"
                with open(Path(self.config.workdir, submission_file), "w") as file:
                    file.write(submission_result)
                ordered_files.append(submission_file)

                # Create the test file.
                execution = self.get_execution(context)
                context_result = context_template.render(
                    execution=execution,
                    code_identifier=self.identifier,
                    output_file=str(Path(self.config.workdir, 'output.txt')).replace("\\", "/"),
                    additionals=context.additional,
                    FunctionType=FunctionType,
                    ValueType=ValueType,
                    context_id=id_,
                    has_top_level=self.language_config.supports_top_level_functions(),
                    name=submission_name,
                    before=context.before.get(self.config.programming_language),
                    after=context.after.get(self.config.programming_language)
                )
                with open(Path(self.config.workdir, self._context_name(id_)), "w") as file:
                    file.write(context_result)
                ordered_files.append(self._context_name(id_))
                context_ids.append(id_)

        return context_ids, ordered_files

    def _submission_name(self, name):
        return f"{name}.{self.language_config.file_extension()}"

    def compile(self, ordered_files) -> ExecutionResult:
        command = self.language_config.compilation_command(ordered_files)
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

        command = self.language_config.execution_command(context_id)
        p = subprocess.run(command, input=stdin_, timeout=timeout, text=True, capture_output=True,
                           cwd=self.config.workdir)
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
    'java': JavaConfig,
    'haskell': HaskellConfig
}


def get_runner(config: Config) -> BaseRunner:
    """Get the runner for the specified language."""
    return ConfigurableRunner(config, CONFIGS[config.programming_language]())
