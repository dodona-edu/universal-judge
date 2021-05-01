class Counter:

    def __init__(self):
        self.counter = 0

    def add(self):
        self.counter += 1
        return self

    def get(self):
        return self.counter
