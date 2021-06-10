class Store:

    def __init__(self, a, b):
        self.a, self.b = a, b

    def apply(self, function):
        return function(self.a, self.b)


def add(a, b):
    return a + b


def create_store(constructor, a, b):
    return constructor(a, b)
