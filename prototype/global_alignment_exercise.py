import os
import random

from Bio import SeqIO
from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord

from dodona_exercise import DodonaExercise


def mutateSequence(seq, events, mutationTypeRate=None):
    # check validity of mutation type rates
    if mutationTypeRate is None:
        # rate of insertions, deletions and substutitions
        mutationTypeRate = [1 / 3, 1 / 3, 1 / 3]

    # convert mutation type rates into accumulative values
    mutationTypeRate = list(mutationTypeRate)
    for i in range(1, len(mutationTypeRate)):
        mutationTypeRate[i] += mutationTypeRate[i - 1]

    # check if mutation type rates accumulate to one (normalization property)
    assert abs(1.0 - mutationTypeRate[-1]) < 1e-6

    # simulate mutation events
    for _ in range(events):
        r = random.random()
        if r < mutationTypeRate[0]:
            # simulate insertion
            pos = random.randint(0, len(seq))
            seq = seq[:pos] + random.choice('ACDEFGHIKLMNPQRSTVWY') + seq[pos:]
        elif r < mutationTypeRate[1]:
            # simulate deletion
            pos = random.randint(0, len(seq) - 1)
            seq = seq[:pos] + seq[pos + 1:]
        else:
            # simulate mutation
            pos = random.randint(0, len(seq) - 1)
            seq = seq[:pos] + random.choice('ACDEFGHIKLMNPQRSTVWY'.replace(seq[pos], '')) + seq[pos + 1:]

    return seq


def randomPeptide(length):
    return ''.join(random.choice('ACDEFGHIKLMNPQRSTVWY') for _ in range(length))


class GlobalAlignment(DodonaExercise):
    def __init__(self):
        self.test_num = 0

    @property
    def name(self):
        return 'global_aligment'

    def get_function(self):
        return ['global_alignment({0})']

    @property
    def initial_parameters(self):
        return 500, 500

    def get_test(self, parameters):
        filename = os.path.join('data', f'data{self.test_num}.fasta')
        self.test_num += 1

        length = parameters[0]
        origseq = randomPeptide(length)
        insertionRate = random.random()
        deletionRate = random.random() * (1 - insertionRate)
        substitutionRate = 1 - insertionRate - deletionRate

        secondseq = mutateSequence(origseq,
                                   int(length * (0.1 + random.random() * 0.9)),
                                   mutationTypeRate=[insertionRate, deletionRate, substitutionRate])
        if len(secondseq) < parameters[1]:
            secondseq += randomPeptide(parameters[1] - len(secondseq))
        else:
            secondseq = secondseq[:parameters[1]]

        seq0 = SeqRecord(Seq(origseq), id='seq0', description='')
        seq1 = SeqRecord(Seq(secondseq), id='seq1', description='')
        SeqIO.write((seq0, seq1), filename, 'fasta')
        return f"'{filename}'",

    @property
    def symbolic_time_complexity(self):
        return 'm * n', 'm', 'n'

    def time_complexity(self, args):
        return args[0] * args[1], args[0], args[1]

    @property
    def symbolic_memory_complexity(self):
        return 'min(m, n)', 'm * n', 'm', 'n'

    def memory_complexity(self, args):
        return min(args[0], args[1]), args[0] * args[1], args[0], args[1]

    @property
    def submissions_url(self):
        return 'https://dodona.ugent.be/nl/courses/65/exercises/2019589367/submissions/'


    @property
    def preload_code(self):
        return ''

    @property
    def language(self):
        return 'python3'
