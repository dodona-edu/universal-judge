import psutil


class MemoryLimit:
    def __init__(self, bytes=None, process_id=None):
        self.bytes = bytes
        self.process = psutil.Process(process_id)
        self.limit = None

    def __enter__(self):
        self.limit = self.process.memory_info().rss + self.bytes if self.bytes is not None else None
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.limit = None
        return self

    @property
    def exceeded(self):
        return self.limit is not None and self.process.memory_info().rss > self.limit
