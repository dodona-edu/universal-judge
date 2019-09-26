from exercise import Exercise
from math import log2
from random import randint


class LogSearch(Exercise):
    @property
    def name(self):
        return 'log_search'

    def get_function(self):
        return ['a = {0}\nb = {1}',
                'zoeken(a, b)']

    @property
    def initial_parameters(self):
        return 500000,

    def get_test(self, parameters):
        length = parameters[0]
        return repr(randint(0, length)), repr(list(range(length)))

    @property
    def symbolic_time_complexity(self):
        return 'log(n)', 'n'

    def time_complexity(self, args):
        return log2(args[0]), args[0]

    @property
    def symbolic_memory_complexity(self):
        return '1', 'n',

    def memory_complexity(self, args):
        return 1, args[0],

    @property
    def language(self):
        return 'python3'

    def get_submissions(self):
        with open('log_search.py') as infile:
            return {'1': [infile.read()]}
