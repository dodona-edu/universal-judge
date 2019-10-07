# DSL types.
#
# Eventually, the DSL should look a bit like this:
#
# with Judgement() as judgement:
#
# end
from typing import List

from dodona.partial_output import *
from contextlib import AbstractContextManager


class Judgment(AbstractContextManager):
    def __init__(self):
        self.children = []


class Tab(AbstractContextManager):
    def __init__(self, parent: Judgment, name: str, hidden: bool = None):
        self.parent = parent
        self.children = []
        self.name = name
        self.hidden = hidden

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.parent.children.append(self)


class Context(AbstractContextManager):
    def __init__(self, parent: Tab, description: Message = None):
        self.parent = parent
        self.children = []
        self.description = description

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.parent.children.append(self)


class Testcase(AbstractContextManager):
    def __init__(self, parent: Context, description: Message):
        self.parent = parent
        self.children = []
        self.description = description

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.parent.children.append(self)


class Test(AbstractContextManager):
    """
    Define a test case. In each test case, you can define the input and expected output.
    """
    def __init__(self, parent: Testcase):
        self.parent = parent
        self.description = None
        self.expected = None
        self.stdin = None
        self.stdout = None
        self.file = None
        self.language = None

    def description(self, description: Message):
        self.description = description

    def expected(self, expected: str):
        """
        Define the expected text that will be shown on Dodona. If not set, the judge will
        attempt to use the value specified as stdout.
        """
        self.expected = str

    def stdin(self, input_: str, file=False):
        """
        Define the input for the programme.
        :param file: If true, the value from input_ will be used as a file.
        :param input_: The value to be given to stdin, or the path to a file.
        :return:
        """
        self.stdin = input_

    def stdout(self, expected: str):
        self.stdout = expected

    def stderr(self, expected: str):
        self.expected = expected

    def evaluator(self, file: str, lanugage: str):
        self.file = file
        self.language = lanugage
