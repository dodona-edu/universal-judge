"""
The partial format should automatically close output, but doesn't for some reason.
This module contains a class that will collect the output, and override it on on
testcase level if needed.
"""
import dataclasses
import logging
from collections import defaultdict
from typing import List, Union, Optional, Generator

from tested.configs import Bundle
from tested.dodona import Update, Status, report_update, StatusMessage, \
    CloseTab, CloseContext, CloseTestcase, StartJudgment, CloseJudgment, \
    StartTab, StartContext, StartTestcase

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

    :ivar tree_stack: A stack of the structure. This enables us to automatically
                      close stuff if needed.
    :ivar prepared: The fallback.
    :ivar bundle: The configuration bundle.
    :ivar tab: The index of the latest completed tab.
    :ivar context: The index of the latest completed context.
    """
    __slots__ = ["tree_stack", "collected", "prepared", "bundle", "tab", "context"]

    def __init__(self, bundle: Bundle):
        self.tree_stack: List[str] = []
        self.collected = False
        self.prepared = _ExpectedJudgment()
        self.bundle = bundle
        self.tab = -1
        self.context = -1
        from .evaluation import prepare_evaluation
        prepare_evaluation(bundle, self)

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

    def prepare_context(self,
                        update: Union[StartContext, CloseContext, List[Update]],
                        tab_index: int, context_index: int):
        assert not self.collected, "OutputManager already finished!"
        if isinstance(update, StartContext):
            self.prepared.tabs[tab_index].contexts[context_index].start = update
        elif isinstance(update, CloseContext):
            self.prepared.tabs[tab_index].contexts[context_index].end = update
        else:
            assert isinstance(update, list)
            assert all((not isinstance(x, (
                StartTab, CloseTab, StartContext, CloseContext, StartJudgment,
                CloseJudgment)) for x in update))
            self.prepared.tabs[tab_index].contexts[context_index].content = update

    def _add(self, command: Update):
        """
        Add a command. If the command is not a close command, the buffer will be
        sent. If the command is a close command,
        """
        assert not self.collected, "OutputManager already finished!"
        action, type_ = command.command.split("-")
        if action == "start":
            self.tree_stack.append(type_)
        elif action == "close":
            previous = self.tree_stack.pop()
            assert previous == type_, "Close same type"

        report_update(self.bundle.out, command)
        return action

    def add(self, command: Update):
        assert not isinstance(command,
                              (StartTab, StartContext, CloseContext, CloseTab))
        assert not self.collected, "OutputManager already finished!"
        self._add(command)

    def add_tab(self, update: Union[StartTab, CloseTab], tab_index: int):
        assert not self.collected, "OutputManager already finished!"
        action = self._add(update)
        self.tab = tab_index
        if action == "close":
            del self.prepared.tabs[tab_index]

    def add_context(self, update: Union[StartContext, CloseContext],
                    context_index: int):
        assert not self.collected, "OutputManager already finished!"
        self.context = context_index
        action = self._add(update)
        if action == "close":
            del self.prepared.tabs[self.tab].contexts[context_index]

    def terminate(self, status: Status):
        """Terminates the collector and writes everything."""
        assert not self.collected, "OutputManager already finished!"

        to_add = self._get_to_add()
        for added in to_add:
            modified = _replace_status(added, status)
            self._add(modified)

        # We should have a completed tree by now.
        assert self.tree_stack == []

    def clean_finish(self):
        """Assert that the collector has terminated cleanly."""
        assert self.tree_stack == []

    def _get_to_add(self) -> List[Update]:
        # Determine which default still need to written.
        assert self.tab < len(self.bundle.plan.tabs)
        assert self.context < len(self.bundle.plan.tabs[self.tab].contexts)

        if self.context + 1 == len(self.bundle.plan.tabs[self.tab].contexts):
            self.context = 0
            self.tab += 1
        else:
            self.context += 1

        if self.tab == len(self.bundle.plan.tabs):
            return []

        to_write = []
        if self.tab == -1:
            self.tab += 1
            to_write.append(self.prepared.tabs[self.tab].start)
        # Do remainder of current tab.
        for c in range(self.context, len(self.bundle.plan.tabs[self.tab].contexts)):
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
            for c in range(0, len(self.bundle.plan.tabs[t].contexts)):
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


def _replace_status(t: Update, status: Status) -> Update:
    try:
        # noinspection PyDataclass
        return dataclasses.replace(t, status=StatusMessage(status))
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

    def _generate(self, end: Optional[CloseTestcase])\
            -> Generator[Update, None, None]:
        if self.content:
            yield self.start
            yield from self.content
            if end is not None:
                yield end

    def as_list(self, end: Optional[CloseTestcase]) -> List[Update]:
        return list(self._generate(end))
