import time
from threading import Thread

import psutil


class Sampler(Thread):
    def __init__(self, process_id):
        Thread.__init__(self)
        self._process = psutil.Process(process_id)
        self._stopped = False
        self._samples = []

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stop()

    def run(self):
        while not self._stopped:
            self._samples.append((time.time(), self._process.memory_info().rss))
            # First 3 seconds: take a sample every 10ms
            if len(self._samples) < 300:
                time.sleep(0.01)
            else:
                time.sleep(0.05)

    def stop(self):
        self._stopped = True

    @property
    def samples(self):
        return self._samples
