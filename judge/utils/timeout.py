import time


class Timeout:
    def __init__(self, seconds=None):
        self.seconds = seconds
        self.expire = None

    def __enter__(self):
        self.expire = time.perf_counter() + self.seconds if self.seconds else None
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.expire = None
        return self

    @property
    def expired(self):
        return self.expire is not None and time.perf_counter() > self.expire
