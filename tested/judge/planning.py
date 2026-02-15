"""
This module decides what and when things are executed.
"""

import time
from enum import Enum, auto
from pathlib import Path
from typing import cast

from attrs import define, field

from tested.configs import Bundle
from tested.dodona import AnnotateCode, Message, Status
from tested.languages.conventionalize import execution_name
from tested.languages.language import FileFilter
from tested.testsuite import ContentPath, Context, EmptyChannel, MainInput


@define
class CompilationResult:
    status: Status
    messages: list[Message] = field(factory=list)
    annotations: list[AnnotateCode] = field(factory=list)
    reported: bool = False


@define
class PlannedContext:
    """Identifies a context by its position in the test suite."""

    context: Context
    tab_index: int
    context_index: int


@define(frozen=True)
class DynamicallyGeneratedFile:
    path: str
    content: ContentPath | str


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
        potential = [c.context.get_stdin(resources) for c in self.contexts]
        return "".join(p for p in potential if p)

    def has_main_testcase(self) -> bool:
        return self.contexts[0].context.has_main_testcase()

    def has_exit_testcase(self) -> bool:
        return self.contexts[-1].context.has_exit_testcase()

    def get_dynamically_generated_files(self) -> list[DynamicallyGeneratedFile]:
        generated_files = set()

        for context in self.contexts:
            for testcase in context.context.testcases:
                if (
                    isinstance(testcase.input, MainInput)
                    and testcase.input.stdin != EmptyChannel.NONE
                    and testcase.input.stdin.is_dynamically_generated()
                    and testcase.input.stdin.path is not None
                ):
                    generated_files.add(
                        DynamicallyGeneratedFile(
                            path=testcase.input.stdin.path,
                            content=testcase.input.stdin.content,
                        )
                    )

                for input_file in testcase.input_files:
                    if (
                        input_file.is_dynamically_generated()
                        and input_file.path is not None
                    ):
                        generated_files.add(
                            DynamicallyGeneratedFile(
                                path=input_file.path,
                                content=input_file.content,
                            )
                        )

        return sorted(generated_files, key=lambda f: (f.path, f.content))


@define
class ExecutionPlan:
    units: list[PlannedExecutionUnit]
    common_directory: Path  # The folder in which we will execute.
    selector: str | None

    # When the execution has started
    max_time: float
    start_time: float

    # Stuff that is set after the plan has been made.
    files: list[str] | FileFilter  # The files we need for execution.

    def remaining_time(self) -> float:
        return self.max_time - (time.perf_counter() - self.start_time)


class PlanStrategy(Enum):
    OPTIMAL = auto()
    TAB = auto()
    CONTEXT = auto()


def _flattened_contexts_to_units(
    flattened_contexts: list[PlannedContext],
) -> list[list[PlannedContext]]:
    contexts_per_unit = []
    current_unit_contexts = []

    for planned in flattened_contexts:
        # If we get stdin, start a new execution unit.
        if (
            planned.context.has_main_testcase()
            and cast(MainInput, planned.context.testcases[0].input).stdin
            != EmptyChannel.NONE
        ):
            if current_unit_contexts:
                contexts_per_unit.append(current_unit_contexts)
            current_unit_contexts = []

        current_unit_contexts.append(planned)

        if planned.context.has_exit_testcase():
            contexts_per_unit.append(current_unit_contexts)
            current_unit_contexts = []

    if current_unit_contexts:
        contexts_per_unit.append(current_unit_contexts)

    return contexts_per_unit


def plan_test_suite(
    bundle: Bundle, strategy: PlanStrategy
) -> list[PlannedExecutionUnit]:
    """
    Transform a test suite into a list of execution units.

    :param strategy: Which strategy to follow when planning the units.
    :param bundle: The configuration
    :return: A list of planned execution units.
    """

    # First, flatten all contexts into a single list.
    if strategy == PlanStrategy.OPTIMAL:
        flattened_contexts = []
        for t, tab in enumerate(bundle.suite.tabs):
            for c, context in enumerate(tab.contexts):
                flattened_contexts.append(
                    PlannedContext(context=context, tab_index=t, context_index=c)
                )
        flattened_contexts_list = [flattened_contexts]
    elif strategy == PlanStrategy.TAB:
        flattened_contexts_list = []
        for t, tab in enumerate(bundle.suite.tabs):
            flattened_contexts = []
            for c, context in enumerate(tab.contexts):
                flattened_contexts.append(
                    PlannedContext(context=context, tab_index=t, context_index=c)
                )
            flattened_contexts_list.append(flattened_contexts)
    else:
        assert strategy == PlanStrategy.CONTEXT
        flattened_contexts_list = []
        for t, tab in enumerate(bundle.suite.tabs):
            for c, context in enumerate(tab.contexts):
                flattened_contexts = [
                    PlannedContext(context=context, tab_index=t, context_index=c)
                ]
                flattened_contexts_list.append(flattened_contexts)

    flattened_units = []
    for flattened_contexts in flattened_contexts_list:
        for contexts in _flattened_contexts_to_units(flattened_contexts):
            flattened_units.append(
                PlannedExecutionUnit(
                    contexts=contexts,
                    name=execution_name(bundle.language, len(flattened_units)),
                    index=len(flattened_units),
                )
            )

    return flattened_units
