class Counter:
    def __init__(self):
        self.counter = 0

    def add(self):
        self.counter += 1

    def get(self):
        if True:
            return self.counter
        return 0
