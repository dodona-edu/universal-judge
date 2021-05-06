from threading import current_thread
from time import monotonic
from typing import NamedTuple

collect_measures = False
timings = []


class StageEntry(NamedTuple):
    thread_name: str
    stage_name: str
    start_time: float
    is_start: bool = True


def _get_thread_name():
    return current_thread().getName()


def collect_timings(collect: bool = False):
    global collect_measures
    collect_measures = collect


def new_stage(stage: str):
    global collect_measures
    global timings
    if collect_measures:
        timings.append(StageEntry(thread_name=_get_thread_name(), stage_name=stage,
                                  is_start=True, start_time=monotonic()))


def end_stage(stage: str):
    global collect_measures
    global timings
    if collect_measures:
        timings.append(StageEntry(thread_name=_get_thread_name(), stage_name=stage,
                                  is_start=False, start_time=monotonic()))
