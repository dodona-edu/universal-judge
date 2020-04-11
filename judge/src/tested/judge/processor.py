"""
Module responsible for orchestrating output of random stuff.
"""
import heapq
import logging
from dataclasses import dataclass, field
from threading import Condition
from typing import Tuple, List, Optional, Callable

from tested.dodona import Status
from tested.testplan import Plan

_logger = logging.getLogger(__name__)


@dataclass(frozen=True, order=True)
class _Item:
    identifier: int
    item: Optional[Callable[[], None]] = field(compare=False)

    def is_next(self, comparison: Optional[int]) -> bool:
        return self.identifier == comparison or comparison is None


def _pop_or_none(heap):
    return heapq.heappop(heap) if heap else None


class ResultProcessor:
    __slots__ = ["_waiting_on", "_processing_condition", "_queue", "_plan", "_tab"]

    def __init__(self, plan: Plan, tab_index: int):
        self._waiting_on: Optional[int] = 0
        self._processing_condition: Condition = Condition()
        self._queue: List[_Item] = []
        self._plan: Plan = plan
        self._tab = tab_index

    def _push(self, item: _Item):
        heapq.heappush(self._queue, item)

    def _pop(self) -> Optional[_Item]:
        try:
            return heapq.heappop(self._queue)
        except IndexError:
            return None

    def submit(self, id_: int, result: Callable[[], Optional[Status]]):
        """
        Submit a solution. The solution will be processed if all previous ones are
        processed, similar to a TCP buffer.
        """
        with self._processing_condition:
            item = _Item(id_, result)
            _logger.debug(f"Receiving evaluation of {item.identifier}")
            self._push(item)
            self._processing_condition.notify_all()

    def _get_and_process_next(self) -> Optional[Status]:
        with self._processing_condition:
            item = self._pop()
            while item is None or not item.is_next(self._waiting_on):
                if item is not None:
                    _logger.debug(f"Next item in queue is {item.identifier}")
                    _logger.debug(f"Was waiting on {self._waiting_on}.")
                    self._push(item)
                else:
                    _logger.debug(f"Queue is empty, waiting on {self._waiting_on}.")
                self._processing_condition.wait()
                _logger.debug("Processor is activated, getting next item.")
                item = self._pop()
            self._increment()

        if item is not None:
            return self._process(item)
        else:
            return None

    def processing_thread(self):
        while self._waiting_on is not None:
            r = self._get_and_process_next()
            if r == Status.TIME_LIMIT_EXCEEDED:
                _logger.info("Time limit exceeded. Stopping processing.")
                return
        _logger.debug("Result processing completed.")

    def _increment(self):
        """
        Increment to the next awaiting item. Should only be called when the
        appropriate locks are present.
        """
        if self._waiting_on == len(self._plan.tabs[self._tab].contexts) - 1:
            self._waiting_on = None
        else:
            self._waiting_on += 1

    def _process(self, item: _Item) -> Optional[Status]:
        _logger.debug(f"Processing {item.identifier}")
        return item.item()
