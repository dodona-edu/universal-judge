from dataclasses import dataclass, field
from threading import current_thread
from time import monotonic
from typing import NamedTuple, List, Dict, Optional, Tuple

from tested.internationalization import get_i18n_string


class StageEntry(NamedTuple):
    thread_name: str
    stage_name: str
    timestamp: float
    is_start: bool = True
    sub_stage: bool = False


@dataclass
class SubStageTiming:
    sub_stage_name: str
    duration: float = -1


@dataclass
class StageTiming:
    stage_name: str
    duration: float = -1
    sub_stages: List[SubStageTiming] = field(default_factory=list)


collect_measures: bool = False
timings: List[StageEntry] = []

start_time = monotonic()


def _get_thread_name():
    return current_thread().getName()


def collect_timings(collect: bool = False):
    global collect_measures
    collect_measures = collect


def is_collecting():
    return collect_measures


def end_stage(stage: str, sub_stage: bool = False):
    global collect_measures
    global timings
    if collect_measures:
        timings.append(StageEntry(thread_name=_get_thread_name(), stage_name=stage,
                                  is_start=False, timestamp=monotonic(),
                                  sub_stage=sub_stage))


def group_by_thread() -> Dict[str, List[StageEntry]]:
    global timings
    timings_by_thread = {}
    for entry in timings:
        try:
            timings_by_thread[entry.thread_name].append(entry)
        except KeyError:
            timings_by_thread[entry.thread_name] = [entry]
    return timings_by_thread


def new_stage(stage: str, sub_stage: bool = False):
    global collect_measures
    global timings
    if collect_measures:
        timings.append(StageEntry(thread_name=_get_thread_name(), stage_name=stage,
                                  is_start=True, timestamp=monotonic(),
                                  sub_stage=sub_stage))


def calculate_stages():
    groups = group_by_thread()
    stages: List[StageTiming] = []
    # Calculate interval durations
    for group in groups.values():
        stage: Optional[StageTiming] = None
        stage_start: float = 0.0
        sub_stage_start: float = 0.0
        for row in group:
            if row.sub_stage:
                if row.is_start:
                    if stage.sub_stages and stage.sub_stages[-1].duration < 0:
                        stage.sub_stages[
                            -1].duration = row.timestamp - sub_stage_start
                    sub_stage_start = row.timestamp
                    stage.sub_stages.append(SubStageTiming(row.stage_name))
                pass
            else:
                if row.is_start:
                    if stage is not None:
                        if stage.sub_stages and stage.sub_stages[-1].duration < 0:
                            stage.sub_stages[
                                -1].duration = row.timestamp - sub_stage_start
                        stage.duration = row.timestamp - stage_start
                        stages.append(stage)
                    stage_start = row.timestamp
                    stage = StageTiming(row.stage_name)
                else:
                    stage.duration = row.timestamp - stage_start
                    if stage.sub_stages and stage.sub_stages[-1].duration < 0:
                        stage.sub_stages[
                            -1].duration = row.timestamp - sub_stage_start
                    stages.append(stage)
                    stage = None
    # Combine timings from the same stages
    combined: Dict[str, Tuple[StageTiming, Dict[str, SubStageTiming]]] = {}
    for stage in stages:
        try:
            combined[stage.stage_name][0].duration += stage.duration
        except KeyError:
            combined[stage.stage_name] = StageTiming(stage.stage_name,
                                                     stage.duration), {}
        finally:
            sub_stages = combined[stage.stage_name][1]
            for sub_stage in stage.sub_stages:
                try:
                    sub_stages[sub_stage.sub_stage_name].duration += stage.duration
                except KeyError:
                    sub_stages[sub_stage.sub_stage_name] = SubStageTiming(
                        sub_stage.sub_stage_name, sub_stage.duration)
    # Include substages
    for stage, sub_stages in combined.values():
        stage.sub_stages = list(sub_stages.values())
    return (stage for stage, _ in combined.values())


def pretty_print_timings():
    global start_time
    lines = []
    for stage in calculate_stages():
        stage_name = get_i18n_string(f"timings.{stage.stage_name}")
        lines.append("{:s}: {:0.03f} s".format(stage_name, stage.duration))
        for sub_stage in stage.sub_stages:
            sub_stage_name = get_i18n_string(f"timings.{sub_stage.sub_stage_name}")
            lines.append("    {:s}: {:0.03f} s".format(sub_stage_name,
                                                       sub_stage.duration))
    end_time = monotonic()
    lines.extend(["", "{:s}: {:0.03f} s".format(get_i18n_string(f"timings.total"),
                                                end_time - start_time)])
    return '\n'.join(lines)
