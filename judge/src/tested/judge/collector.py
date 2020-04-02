"""
The partial format should automatically close output, but doesn't for some reason.
This module contains a class that will collect the output, and override it on on
testcase level if needed.
"""
import dataclasses
import logging
from typing import IO, Optional, List, Tuple

from tested.configs import Bundle
from tested.dodona import Update, Status, close_for, report_update, StatusMessage, \
    CloseTab, CloseContext, CloseTestcase

_logger = logging.getLogger(__name__)


class OutputManager:
    """
    Manages and simulates the full output format, while still using the partial
    output format. The manager supports the notion of "fallback", which means the
    output is first generated in its entirety, while afterwards the updates replace
    the output. For example, a testplan with following structure:

    plan:
        tab1:
            context1:
                testcase1
                testcase2
            context2:
                testcase1
                testcase2

    The fallback will generate output for every testcase. If after execution only
    context 1 was run, the output for context 2 will still be shown.

    The buffer of commands is emptied as soon as possible. The buffer is emptied
    every time a non-close command is issued. The reasoning for this is as follows:
    close commands cannot be output immediately, since we might need to inject
    updates from the fallback before showing the close command.

    For example, if we need to show the fallback for context 2, we cannot output
    the "close-judgment" command yet.

    The collector has a few fields:

    :ivar commands: The buffer with updates.
    :ivar tree_stack: A stack of the structure. This enables us to automatically
                      close stuff if needed.
    :ivar prepared: The fallback.
    :ivar output: Where the commands will be written to.
    :ivar max_tab: The index of the latest completed tab.
    :ivar max_context: The index of the latest completed context.
    :ivar max_testcase: The index of the latest completed testcase.
    """
    __slots__ = ["commands", "tree_stack", "collected", "prepared", "output",
                 "max_tab", "max_context", "max_testcase"]

    def __init__(self, bundle: Bundle):
        self.commands: List[Tuple[Update, Optional[int]]] = []
        self.tree_stack = []
        self.collected = False
        self.prepared = []
        self.output = bundle.out
        self.max_tab = 0
        self.max_context = 0
        self.max_testcase = 0
        from .evaluation import prepare_evaluation
        prepare_evaluation(bundle, self)

    def prepare(self, tab_index: Optional[int], context_index: Optional[int],
                testcase_index: Optional[int], results: List[Update]):
        self.prepared.append(((tab_index, context_index, testcase_index), results))

    def add(self, command: Update, id_: Optional[int] = None):
        """
        If a close command is send, all saved commands are output, to make sure the
        output reaches Dodona as soon as possible.
        """
        assert not self.collected, "OutputManager already finished!"
        action, type_ = command.command.split("-")
        if action == "start":
            self.tree_stack.append(type_)
        elif action == "close":
            previous = self.tree_stack.pop()
            assert previous == type_, "Close same type"
            self._save_closed(command, id_)

        self.commands.append((command, id_))

        if not action == "close":
            # Flush buffer.
            for command, ids in self.commands:
                report_update(self.output, command)
            self.commands = []

    def terminate(self, status: Status, id_: Optional[int] = None):
        assert not self.collected, "OutputManager already finished!"
        for i, to_close in enumerate(reversed(self.tree_stack), 1):
            try:
                # noinspection PyArgumentList
                command = close_for(to_close)(status=StatusMessage(status))
            except TypeError:
                command = close_for(to_close)()
            if i == len(self.tree_stack):
                self.commands.append((command, id_))
            else:
                self.commands.append((command, None))
        self.tree_stack.clear()

    def _save_closed(self, command: Update, id_: Optional[int]):
        if isinstance(command, CloseTab):
            self.max_tab = max(self.max_tab, id_ or 0)
        elif isinstance(command, CloseContext):
            self.max_context = max(self.max_context, id_ or 0)
        elif isinstance(command, CloseTestcase):
            self.max_testcase = max(self.max_testcase, id_ or 0)

    def flush(self, to: IO):
        assert not self.collected, "OutputManager already finished!"
        assert not self.tree_stack, f"All outputs should be closed, " \
                                    f"got {self.tree_stack} "

        from_, at = self.determine_handled()
        from_ = min(from_, len(self.prepared))
        added_commands = _prepare(self.prepared[from_ + 2:])[:-2]
        assert not added_commands or added_commands[0][
            0].command.startswith("start-")
        self.commands[-at:-at] = added_commands

        for command, ids in self.commands:
            report_update(to, command)

        self.collected = True

    def determine_handled(self) -> Tuple[int, int]:
        tb = self.max_tab
        co = self.max_context
        tc = self.max_testcase
        indices = [i for i, x in self.prepared]
        zero = next((i for i, x in enumerate((tb, co, tc), 2) if i), 0)
        try:
            return indices.index((tb, co, tc)), zero
        except ValueError:
            return len(self.prepared), 0


def _prepare(collected: List[Update]) -> List[Tuple[Update, None]]:
    result = []
    for _, collector in collected:
        adjusted = []
        for x in collector:
            try:
                # noinspection PyDataclass
                adjusted.append((
                    dataclasses.replace(x, status=StatusMessage(
                        Status.TIME_LIMIT_EXCEEDED
                    )), None))
            except TypeError:
                adjusted.append((x, None))
        result.extend(adjusted)
    return result


class UpdateCollector:
    """
    A class to collect updates but not print them.
    """
    __slots__ = ["start", "content", "id"]

    def __init__(self, start: Update):
        if not start.command.startswith("start"):
            raise ValueError("The collector can only be used with start commands.")
        self.start = start
        self.content = []

    def add(self, update: Update, id_: Optional[int] = None):
        self.content.append((update, id_))

    def end(self, to: OutputManager, end: Update, id_: int):
        if not end.command.startswith("close"):
            raise ValueError(f"The collector can only be used with close commands, "
                             f"got {end.command}")
        _, start_type = self.start.command.split("-")
        _, end_type = end.command.split("-")
        if start_type != end_type:
            raise ValueError(f"The start and close types of the commands must "
                             f"match, got {start_type} and {end_type}.")
        if self.content:
            to.add(self.start, id_)
            for command, c_id in self.content:
                to.add(command, c_id)
            to.add(end, id_)

    def prepare_end(self,
                    to: OutputManager,
                    end: Update,
                    tab_index: Optional[int],
                    context_index: Optional[int],
                    testcase_index: Optional[int]):
        if not end.command.startswith("close"):
            raise ValueError(f"The collector can only be used with close commands, "
                             f"got {end.command}")
        _, start_type = self.start.command.split("-")
        _, end_type = end.command.split("-")
        if start_type != end_type:
            raise ValueError(f"The start and close types of the commands must "
                             f"match, got {start_type} and {end_type}.")
        if self.content:
            to.prepare(tab_index, context_index, testcase_index,
                       [self.start] + [c for c, _ in self.content] + [end]
                       )
