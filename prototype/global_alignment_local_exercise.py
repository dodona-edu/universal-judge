import os

from global_alignment_exercise import GlobalAlignment


class GlobalAlignmentLocal(GlobalAlignment):
    @property
    def name(self):
        return 'global_alignment_local'

    def get_submissions(self):
        result = {}
        for file in os.listdir('global_alignment_local_submissions'):
            with open(os.path.join('global_alignment_local_submissions', file)) as infile:
                result[file.replace('.py', '')] = [infile.read()]
        return result
