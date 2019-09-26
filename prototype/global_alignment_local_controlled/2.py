from Bio import SeqIO
from Bio.SubsMat.MatrixInfo import blosum62


def bl62(a1, a2):
    return blosum62.get((a1, a2)) or blosum62.get((a2, a1)) or 0


INDEL = 5

fst = lambda p: p[0]
snd = lambda p: p[1]


def _middle_edge(vseq, hseq):
    midd = len(hseq) // 2

    left = [-INDEL * i for i in range(len(vseq) + 1)]
    for c in range(1, midd + 1):
        left = [-INDEL * c] + left
        for r in range(1, len(vseq) + 1):
            left[r] = max(left[r - 1] - INDEL
                          , left[r] + bl62(hseq[c - 1], vseq[r - 1])
                          , left[r + 1] - INDEL
                          )
        left = left[:-1]

    right = [-INDEL * i for i in range(len(vseq) + 1)]
    for c in range(1, len(hseq) - midd):
        right = [-INDEL * c] + right
        for r in range(1, len(vseq) + 1):
            right[r] = max(right[r - 1] - INDEL
                           , right[r] + bl62(hseq[-c], vseq[-r])
                           , right[r + 1] - INDEL
                           )
        right = right[:-1]

    right = right[::-1]

    straight = max(((l + r - INDEL, i, 0)
                    for i, (l, r) in enumerate(zip(left, right))),
                   key=fst)
    diagonal = max(((l + r + bl62(vseq[i], hseq[midd]), i, 1)
                    for i, (l, r) in enumerate(zip(left, right[1:]))),
                   key=fst)
    score, node, diff = max(straight, diagonal, key=fst)
    return score, (node, midd), (node + diff, midd + 1)


def linear_space_alignment(vseq, hseq):
    if not vseq:
        return -INDEL * len(hseq), hseq, '-' * len(hseq)
    if not hseq:
        return -INDEL * len(vseq), vseq, '-' * len(vseq)
    score, (lr, lc), (rr, rc) = _middle_edge(vseq, hseq)
    _, lv, lh = linear_space_alignment(vseq[:lr], hseq[:lc])
    _, rv, rh = linear_space_alignment(vseq[rr:], hseq[rc:])
    vert = vseq[lr] if lr < len(vseq) and lr != rr else '-'
    return score, lv + vert + rv, lh + hseq[lc] + rh


def global_alignment_score(infile):
    seq1, seq2 = [str(rec.seq) for rec in SeqIO.parse(infile, 'fasta')]
    return _middle_edge(seq1, seq2)[0]


def global_alignment(infile):
    seq1, seq2 = [str(rec.seq) for rec in SeqIO.parse(infile, 'fasta')]
    return linear_space_alignment(seq1, seq2)[1:]
