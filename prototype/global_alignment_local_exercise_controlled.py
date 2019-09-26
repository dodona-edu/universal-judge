import os

from global_alignment_exercise import GlobalAlignment


class GlobalAlignmentLocalControlled(GlobalAlignment):
    @property
    def name(self):
        return 'global_alignment_local_controlled_break'

    def get_submissions(self):
        result = {}
        for file in sorted(os.listdir('global_alignment_local_controlled')):
            with open(os.path.join('global_alignment_local_controlled', file)) as infile:
                result[file.replace('.py', '')] = [infile.read()]
        return result
