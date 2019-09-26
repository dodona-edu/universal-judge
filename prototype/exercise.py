class Exercise:
    @property
    def name(self):
        raise NotImplementedError('Exercise does not implement name')

    def get_function(self):
        raise NotImplementedError('Exercise does not implement get_function')

    @property
    def initial_parameters(self):
        raise NotImplementedError("Exercise does not implement initial_parameters")

    def get_test(self, parameters):
        raise NotImplementedError('Exercise does not implement get_test')

    @property
    def symbolic_time_complexity(self):
        raise NotImplementedError('Exercise does not implement symbolic_time_complexity')

    def time_complexity(self, args):
        raise NotImplementedError('Exercise does not implement time_complexity')

    @property
    def symbolic_memory_complexity(self):
        raise NotImplementedError('Exercise does not implement symbolic_memory_complexity')

    def memory_complexity(self, args):
        raise NotImplementedError('Exercise does not implement memory_complexity')

    @property
    def language(self):
        raise NotImplementedError('Exercise does not implement language')

    def get_submissions(self):
        raise NotImplementedError('Exercise does not implement get_submissions')
