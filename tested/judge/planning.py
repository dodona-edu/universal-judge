"""
This module decides what and when things are executed.
"""
import time
from pathlib import Path
from typing import Optional

from attrs import define

from tested.dodona import AnnotateCode, Message, Status
from tested.languages.config import FileFilter
from tested.testsuite import Context

CompilationResult = tuple[list[Message], Status, list[AnnotateCode]]


@define
class PlannedContext:
    """Identifies a context by its position in the test suite."""

    context: Context
    tab_index: int
    context_index: int


@define
class PlannedExecutionUnit:
    """
    Planned contexts are grouped together in series of executions. Each execution
    consists of the contexts that can be executed together in one go.
    """

    contexts: list[PlannedContext]
    # The name of this execution, conventionalized for the language.
    name: str
    # Which position in the execution plan this execution has.
    index: int

    def get_stdin(self, resources: Path) -> str:
        return "\n".join(c.context.get_stdin(resources) or "" for c in self.contexts)

    def has_main_testcase(self) -> bool:
        return self.contexts[0].context.has_main_testcase()

    def has_exit_testcase(self) -> bool:
        return self.contexts[-1].context.has_exit_testcase()


@define
class ExecutionPlan:
    units: list[PlannedExecutionUnit]
    common_directory: Path  # The folder in which we will execute.
    selector: Optional[str]

    # When the execution has started
    max_time: float
    start_time: float

    # Stuff that is set after the plan has been made.
    files: list[str] | FileFilter  # The files we need for execution.

    def remaining_time(self) -> float:
        return self.max_time - (time.perf_counter() - self.start_time)
