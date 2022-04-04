"""
The partial format should automatically close output, but doesn't for some reason.
This module contains a class that will collect the output, and override it on on
testcase level if needed.
"""
import dataclasses
import logging
from collections import defaultdict
from math import floor
from typing import List, Union, Optional, Generator

from tested.configs import Bundle
from tested.dodona import (
    Update,
    Status,
    report_update,
    StatusMessage,
    CloseTab,
    CloseContext,
    CloseTestcase,
    StartJudgment,
    CloseJudgment,
    StartTab,
    StartContext,
    StartTestcase,
    close_for,
    ExtendedMessage,
    EscalateStatus,
    update_size,
    limit_size,
    AnnotateCode,
)
from tested.testplan import Run

_logger = logging.getLogger(__name__)


class _ExpectedContext:
    __slots__ = ["start", "end", "content"]

    def __init__(self):
        self.content = []


class _ExpectedTab:
    __slots__ = ["start", "end", "contexts"]

    def __init__(self):
        self.contexts = defaultdict(lambda: _ExpectedContext())


class _ExpectedJudgment:
    __slots__ = ["start", "end", "tabs"]

    def __init__(self):
        self.tabs = defaultdict(lambda: _ExpectedTab())


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

    The fallback will generate output for every context. This means that if a
    context is started, but stopped after, for example, testcase 5, the rest of the
    testcase will not be shown either. This is to reduce the complexity: TESTed
    determines which testcases are shown at run time, based on the results. It is
    very difficult and error prone to predict which ones are shown and which ones
    are not.

    For example, if we need to show the fallback for context 2, we cannot output
    the "close-judgment" command yet.

    Tabs, contexts and testcases are identified by there number within their parent.
    The context testcase has id 0, while the others start at 1.

    The collector has a few fields:

    :ivar seen_annotations: Set of all seen code annotations, to avoid duplicates
    :ivar tree_stack: A stack of the structure. This enables us to automatically
                      close stuff if needed.
    :ivar prepared: The fallback.
    :ivar bundle: The configuration bundle.
    :ivar tab: The index of the latest completed tab.
    :ivar context: The index of the latest completed context.
    :ivar output_limit: How much bytes we can still write. If this is in danger of
                        being 0, outputs will be truncated. Note that this only
                        counts the "content", not other stuff. You should add a
                        buffer for the other stuff.
    """

    __slots__ = [
        "seen_annotations",
        "tree_stack",
        "collected",
        "prepared",
        "bundle",
        "tab",
        "context",
        "output_limit",
    ]

    def __init__(self, bundle: Bundle):
        self.seen_annotations: List[AnnotateCode] = list()
        self.tree_stack: List[str] = []
        self.collected = False
        self.prepared = _ExpectedJudgment()
        self.bundle = bundle
        self.tab = -1
        self.context = -1
        self.output_limit = int(floor(bundle.config.output_limit * 0.8))
        from .evaluation import prepare_evaluation

        prepare_evaluation(bundle, self)

    def is_full(self) -> bool:
        return self.output_limit <= 0

    def prepare_judgment(self, update: Union[StartJudgment, CloseJudgment]):
        assert not self.collected, "OutputManager already finished!"
        if isinstance(update, StartJudgment):
            self.prepared.start = update
        else:
            assert isinstance(update, CloseJudgment)
            self.prepared.end = update

    def prepare_tab(self, update: Union[StartTab, CloseTab], tab_index: int):
        assert not self.collected, "OutputManager already finished!"
        if isinstance(update, StartTab):
            self.prepared.tabs[tab_index].start = update
        else:
            assert isinstance(update, CloseTab)
            self.prepared.tabs[tab_index].end = update

    def prepare_context(
        self,
        update: Union[StartContext, CloseContext, List[Update]],
        tab_index: int,
        context_index: int,
    ):
        assert not self.collected, "OutputManager already finished!"
        if isinstance(update, StartContext):
            self.prepared.tabs[tab_index].contexts[context_index].start = update
        elif isinstance(update, CloseContext):
            self.prepared.tabs[tab_index].contexts[context_index].end = update
        else:
            assert isinstance(update, list)
            assert all(
                (
                    not isinstance(
                        x,
                        (
                            StartTab,
                            CloseTab,
                            StartContext,
                            CloseContext,
                            StartJudgment,
                            CloseJudgment,
                        ),
                    )
                    for x in update
                )
            )
            self.prepared.tabs[tab_index].contexts[context_index].content = update

    def _add(self, command: Update):
        """
        Add a command. If the command is not a close command, the buffer will be
        sent. If the command is a close command,
        """
        assert not self.collected, "OutputManager already finished!"
        action, type_ = command.command.split("-")
        _logger.debug(f"Adding {command}")
        _logger.debug(f"Stack is {self.tree_stack}")
        if action == "start":
            self.tree_stack.append(type_)
        elif action == "close":
            previous = self.tree_stack.pop()
            assert previous == type_, "Close same type"

        _logger.debug(f"After adding, stack is {self.tree_stack}")
        report_update(self.bundle.out, command)
        return action

    def add(self, command: Update):
        assert not isinstance(command, (StartTab, StartContext, CloseContext, CloseTab))
        assert not self.collected, "OutputManager already finished!"

        # Check if command is code annotations
        if isinstance(command, AnnotateCode):
            # If annotation al ready seen skip annotation
            if command in self.seen_annotations:
                return
            # Annotation not yet seen add to seen list
            else:
                _logger.debug(f"Not seen annotation: {command}")
                self.seen_annotations.append(command)

        size = update_size(command)
        if size > self.output_limit:
            command = limit_size(command, self.output_limit - size)
        self.output_limit -= min(self.output_limit, size)
        self._add(command)

    def add_tab(self, update: Union[StartTab, CloseTab], tab_index: int):
        assert not self.collected, "OutputManager already finished!"
        if isinstance(update, CloseTab) and tab_index >= 0:
            for _, context in sorted(
                self.prepared.tabs[tab_index].contexts.items(), key=lambda x: x[0]
            ):
                self._add(context.start)
                for command in context.content:
                    self._add(command)
                self._add(context.end)

        action = self._add(update)
        self.tab = tab_index
        if action == "close" and tab_index >= 0:
            del self.prepared.tabs[tab_index]

    def add_context(
        self, update: Union[StartContext, CloseContext], context_index: Optional[int]
    ):
        assert not self.collected, "OutputManager already finished!"
        if context_index is None:
            self._add(update)
            return

        self.context = context_index
        action = self._add(update)
        if action == "close":
            del self.prepared.tabs[self.tab].contexts[context_index]

    def terminate(self, status: Union[Status, StatusMessage]):
        """Terminates the collector and writes everything."""
        assert not self.collected, "OutputManager already finished!"

        if isinstance(status, Status):
            status = StatusMessage(enum=status)

        # Ensure we use the given status.
        self._add(EscalateStatus(status=status))
        to_add = self._get_to_add()
        for added in to_add:
            modified = _replace_status(added, status)
            self._add(modified)

        # Add stack.
        for to_close in reversed(self.tree_stack):
            try:
                # noinspection PyArgumentList
                command = close_for(to_close)(status=status)
            except TypeError:
                command = close_for(to_close)()
            self._add(command)
        self.collected = True

    def clean_finish(self):
        """Assert that the collector has terminated cleanly."""
        assert self.tree_stack == []

    def _get_to_add(self) -> List[Update]:
        # Determine which default still need to written.
        assert self.tab < len(self.bundle.plan.tabs)
        tab = self.bundle.plan.tabs[self.tab]
        contexts_count = tab.count_contexts()
        assert self.context < contexts_count

        to_write = []

        if self.context + 1 == contexts_count:
            self.context = 0
            to_write.append(self.prepared.tabs[self.tab].end)
            self.tab += 1
            if self.tab == len(self.bundle.plan.tabs):
                return to_write
            to_write.append(self.prepared.tabs[self.tab].start)
        else:
            self.context += 1

        if self.tab != -1:
            # Do remainder of current tab.
            for c in range(self.context, contexts_count):
                context = self.prepared.tabs[self.tab].contexts[c]
                to_write.append(context.start)
                to_write.extend(context.content)
                to_write.append(context.end)
                del self.prepared.tabs[self.tab].contexts[c]
            to_write.append(self.prepared.tabs[self.tab].end)
            del self.prepared.tabs[self.tab]

        # Do remainder of tabs.
        for t in range(self.tab + 1, len(self.bundle.plan.tabs)):
            to_write.append(self.prepared.tabs[t].start)
            for c in range(0, len(self.prepared.tabs[t].contexts)):
                context = self.prepared.tabs[t].contexts[c]
                to_write.append(context.start)
                to_write.extend(context.content)
                to_write.append(context.end)
                del self.prepared.tabs[t].contexts[c]
            to_write.append(self.prepared.tabs[t].end)
            del self.prepared.tabs[t]

        # Stop judgment.
        to_write.append(self.prepared.end)

        assert all(x is not None for x in to_write)

        return to_write


def _replace_status(t: Update, status: StatusMessage) -> Update:
    try:
        # noinspection PyDataclass
        return dataclasses.replace(t, status=status)
    except TypeError:
        return t


class TestcaseCollector:
    """
    Collects updates for a testcase, but only outputs them if the testcase has
    content (children). This is intended to be used to evaluate testcases: they can
    be started without problem, but if nothing is written during evaluation, they
    will not be shown in Dodona.
    """

    __slots__ = ["start", "content"]

    def __init__(self, start: StartTestcase):
        assert isinstance(start, StartTestcase)
        self.start = start
        self.content = []

    def add(self, update: Update):
        self.content.append(update)

    def to_manager(self, manager: OutputManager, end: Optional[CloseTestcase]):
        assert end is None or isinstance(end, CloseTestcase)
        for command in self._generate(end):
            manager.add(command)

    def _generate(self, end: Optional[CloseTestcase]) -> Generator[Update, None, None]:
        has_text = isinstance(self.start.description, str) and self.start.description
        has_extended = (
            isinstance(self.start.description, ExtendedMessage)
            and self.start.description.description
        )
        if has_text or has_extended:
            yield self.start
            yield from self.content
            if end is not None:
                yield end

    def as_list(self, end: Optional[CloseTestcase]) -> List[Update]:
        return list(self._generate(end))
