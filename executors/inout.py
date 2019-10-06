# Executor for exercises where stdin expects input and receives output in stdout.
import glob
from sys import stderr
from typing import List, Tuple, Dict
from pathlib import Path

from dodona.partial_output import TestData
from tested import Config
from dodona import partial_output as po
from dodona.dodona import report_update
from jupyter import JupyterContext


class Tester:
    config: Config

    def __init__(self, config: Config):
        self.config = config

    def test(self, code, context: JupyterContext):
        """
        Execute the tests.
        """
        pass


class Comparator:

    def compare(self, expected, actual) -> bool:
        pass


class SimpleStringComparator(Comparator):

    def __init__(self, strip=True):
        self.strip = strip

    def compare(self, expected: str, actual: str) -> bool:
        if self.strip:
            return expected.strip() == actual.strip()
        else:
            return expected == actual


class InOutTester(Tester):
    """
    Execute exercises that want input and produce output.
    """

    def in_out_files(self) -> List[str]:
        """
        Get the different input and output files needed for the test.
        EVery input file needs one output file. If not, an error is thrown.
        """
        in_files = sorted(glob.glob(f"{self.config.resources}/*.in"))
        out_files = sorted(glob.glob(f"{self.config.resources}/*.out"))

        in_names = [Path(x).stem for x in in_files]
        out_names = [Path(x).stem for x in out_files]

        assert in_names == out_names
        return in_names

    def get_values(self, file) -> List[str]:
        with open(f"{self.config.resources}/{file}", "r") as f:
            return f.read().splitlines(False)

    def evaluate_code(self, code, input_, expected, context: JupyterContext) -> po.CloseTest:
        # Evaluate the code with input, and produce a result.
        # TODO: evaluate
        # We should:
        # 1. get the user code, load it into the kernel
        # 2. pass stdin to the kernel (the code should be waiting on it)
        # 3. read stdout from the kernel
        #
        # Every test should run it it's own context, so there is no interference.

        context.run()
        messages = context.execute_statements(code,
                                              timeout=self.config.time_limit,
                                              memory_limit=self.config.memory_limit,
                                              std_input=input_)
        context.clean()
        # Collect stdout from messages.
        stdout = []
        errors = []
        for message in messages['iopub']:
            type_ = message['header']['msg_type']
            if type_ == 'stream':
                stream = message['content']['name']
                if stream == 'stdout':
                    stdout.append(message['content']['text'])
                elif stream == 'stderr':
                    errors.append({'ename': 'StdErr', 'evalue' : message['content']['text']})
                else:
                    raise ValueError(f"Unknown type {stream}")
            elif type_ == 'error' and message['content']['traceback']:
                errors.append({
                    'ename': 'RuntimeError',
                    'evalue': message['content']['traceback']
                })

        for message in messages['client']:
            if message['msg_type'] == 'error':
                errors.append(message['content'])

        produced_output = ''.join(stdout)
        if errors and any(e['ename'] == 'TimeoutError' for e in errors):
            status = po.StatusMessage(po.Status.TIME_LIMIT_EXCEEDED)
        elif errors and any(e['ename'] == 'RuntimeError' for e in errors):
            status = po.StatusMessage(po.Status.RUNTIME_ERROR)
            # Print to stderr
            error = next(e for e in errors if e['ename'] == 'RuntimeError')
            m = po.AppendMessage(error)
            report_update(m)
        else:
            comparator = SimpleStringComparator()
            if comparator.compare(expected, produced_output):
                status = po.StatusMessage(po.Status.CORRECT)
            else:
                status = po.StatusMessage(po.Status.WRONG)
        return po.CloseTest(produced_output, status, data=TestData(channel='stdout'))

    def test(self, code, context: JupyterContext):
        # Start judgement
        report_update(po.StartJudgment())

        to_test = self.in_out_files()

        for test in to_test:
            report_update(po.StartTab(title=test))

            inputs = self.get_values(f"{test}.in")
            outputs = self.get_values(f"{test}.out")

            for (input_, expected) in zip(inputs, outputs):
                report_update(po.StartContext())
                report_update(po.StartTestcase(description=input_))
                report_update(po.StartTest(expected=expected))
                result = self.evaluate_code(code, input_, expected, context)
                report_update(result)
                report_update(po.CloseTestcase())
                report_update(po.CloseContext())

            report_update(po.CloseTab())

        report_update(po.CloseJudgment())
