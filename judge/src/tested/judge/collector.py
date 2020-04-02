"""
The partial format should automatically close output, but doesn't for some reason.
This module contains a class that will collect the output, and override it on on
testcase level if needed.
"""
import dataclasses
import logging
from typing import IO, Union, Optional, List, Tuple

from tested.configs import Bundle
from tested.dodona import Update, Status, close_for, report_update, StatusMessage, \
    CloseTab, CloseContext, CloseTestcase

_logger = logging.getLogger(__name__)


class Collector:
    """
    Collect all output and write it at the end. By initiating a collector, all
    testcases are prepared with an unique ID (tab index - context index - testcase
    index). These are overridden when the actual commands are sent.
    """
    __slots__ = ["commands", "tree_stack", "collected", "prepared"]

    def __init__(self):
        self.commands: List[Tuple[Update, Optional[int]]] = []
        self.tree_stack = []
        self.collected = False
        self.prepared = []

    def add_fallback(self, bundle: Bundle):
        from .evaluation import prepare_evaluation
        prepare_evaluation(bundle, self)

    def prepare(self, tab_index: Optional[int], context_index: Optional[int],
                testcase_index: Optional[int], results: List[Update]):
        self.prepared.append(((tab_index, context_index, testcase_index), results))

    def out(self, command: Update, id_: Optional[int] = None):
        assert not self.collected, "Collector already finished!"
        action, type_ = command.command.split("-")
        if action == "start":
            self.tree_stack.append(type_)
        elif action == "close":
            previous = self.tree_stack.pop()
            assert previous == type_, "Close same type"

        self.commands.append((command, id_))

    def terminate(self, status: Status, id_: Optional[int] = None):
        assert not self.collected, "Collector already finished!"
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

    def write(self, to: Union[IO, 'Collector']):
        assert not self.collected, "Collector already finished!"
        assert not self.tree_stack, f"All outputs should be closed, " \
                                    f"got {self.tree_stack} "

        if len(self.commands) > 2:
            if self.prepared:
                from_, at = self.determine_handled()
                from_ = min(from_, len(self.prepared))
                added_commands = _prepare(self.prepared[from_ + 2:])[:-2]
                assert not added_commands or added_commands[0][0].command.startswith("start-")
                self.commands[-at:-at] = added_commands

            for command, ids in self.commands:
                if isinstance(to, Collector):
                    to.out(command, ids)
                else:
                    report_update(to, command)
        else:
            _logger.debug(f"Ignoring {self.commands}, no content.")
        self.collected = True

    def determine_handled(self) -> Tuple[int, int]:
        c = self.commands
        tb = max((i for x, i in c if isinstance(x, CloseTab) and i), default=0)
        co = max((i for x, i in c if isinstance(x, CloseContext) and i), default=0)
        tc = max((i for x, i in c if isinstance(x, CloseTestcase) and i), default=0)
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
