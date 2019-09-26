import numpy as np
from Bio import SeqIO
from Bio.SubsMat.MatrixInfo import blosum62


class GlobalAlignment:
    def __init__(self, sequences, scoringMatrix=None, linearGapPenalty=5):

        # alignment parameters
        self.scoringMatrix = scoringMatrix if scoringMatrix is not None else blosum62
        self.linearGapPenalty = linearGapPenalty

        # sequences to be aligned
        self.sequences = tuple(map(
            lambda seq: str(seq.seq),
            SeqIO.parse(sequences, format='fasta')
        ))

        # dynamic programming matrix
        self.__dp_matrix = None

    def dp_matrix(self):

        if self.__dp_matrix is not None:
            return self.__dp_matrix

        import functools
        @functools.lru_cache(None)
        def score(base1, base2):

            try:
                return self.scoringMatrix[base1, base2]
            except KeyError:
                return self.scoringMatrix[base2, base1]

        # intialize dynamic programming matrix
        # NOTE: use native word size for int to improve speed
        seq1, seq2 = self.sequences
        m, n = len(seq1), len(seq2)
        M = self.__dp_matrix = np.empty((m + 1, n + 1), dtype=int)

        # fill first column
        M[:, 0] = np.fromiter(
            (-r * self.linearGapPenalty for r in range(0, m + 1)),
            dtype=int,
            count=m + 1
        )

        # fill first row
        M[0, 1:] = np.fromiter(
            (-c * self.linearGapPenalty for c in range(1, n + 1)),
            dtype=int,
            count=n
        )

        # fill dynamic programming matrix
        for r in range(1, m + 1):
            for c in range(1, n + 1):
                M[r, c] = max(
                    # insertion
                    M[r - 1, c] - self.linearGapPenalty,
                    # deletion
                    M[r, c - 1] - self.linearGapPenalty,
                    # substitution (match or mismatch)
                    M[r - 1, c - 1] + score(seq1[r - 1], seq2[c - 1])
                )

        return M

    def score(self):

        # take score from bottom right cell of dynamic programming matrix
        return int(self.dp_matrix()[-1, -1])

    def alignment(self):

        # fill dynamic programming matrix (if not already done)
        M = self.dp_matrix()

        # traceback alignments
        seq1, seq2 = self.sequences
        r, c = len(seq1), len(seq2)
        alignment = [[], []]
        while r or c:

            if r and M[r - 1, c] - self.linearGapPenalty == M[r, c]:
                r -= 1
                alignment[0].append(seq1[r])
                alignment[1].append('-')
            elif c and M[r, c - 1] - self.linearGapPenalty == M[r, c]:
                c -= 1
                alignment[0].append('-')
                alignment[1].append(seq2[c])
            else:
                r -= 1
                c -= 1
                alignment[0].append(seq1[r])
                alignment[1].append(seq2[c])

        return tuple(map(lambda a: ''.join(a[::-1]), alignment))


def global_alignment_score(sequences, scoringMatrix=None, linearGapPenalty=5):
    return GlobalAlignment(sequences, scoringMatrix, linearGapPenalty).score()


def global_alignment(sequences, scoringMatrix=None, linearGapPenalty=5):
    return GlobalAlignment(sequences, scoringMatrix, linearGapPenalty).alignment()