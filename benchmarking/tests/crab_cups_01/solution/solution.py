class Cups:

    """
    >>> cups = Cups([3, 8,  9,  1,  2,  5,  4,  6,  7])
    >>> cups
    Cups([3, 8, 9, 1, 2, 5, 4, 6, 7], current=3)
    >>> print(cups)
    (3) 8 9 1 2 5 4 6 7
    >>> cups.order
    '25467389'
    >>> cups.move()
    Cups([2, 8, 9, 1, 5, 4, 6, 7, 3], current=2)
    >>> cups.move()
    Cups([5, 4, 6, 7, 8, 9, 1, 3, 2], current=5)
    >>> cups.move()
    Cups([8, 9, 1, 3, 4, 6, 7, 2, 5], current=8)
    >>> cups.moves(7).order
    '92658374'
    >>> cups.moves(90).order
    '67384529'
    """

    def __init__(self, cups, current=None):

        # remember the current cup
        # NOTE: first cup is the default current cup
        # NOTE: all values are lowered by one to make the modulo operator work
        self.current = (cups[0] if current is None else current)

        # determine the length of the circular list
        self.length = len(cups)

        # dictionary maps elements to their successor in the circular linked list
        self._cups = {cups[-1]:cups[0]}
        for previous, next in zip(cups, cups[1:]):
            self._cups[previous] = next

    def __len__(self):

        return self.length

    def __repr__(self):

        return f'Cups([{", ".join(str(cup) for cup in self.cups())}], current={self.current})'

    def __str__(self):

        return ' '.join(
            f'{cup}' if cup != self.current else f'({cup})'
            for cup in self.cups()
        )

    def get_successor(self, cup):

        return self._cups[cup]

    def set_successor(self, cup, cup2):

        self._cups[cup] = cup2
        return self

    def cups(self, start=None):

        cup = self.current if start is None else start
        for _ in range(self.length):
            yield cup
            cup = self.get_successor(cup)

    @property
    def order(self):

        return ''.join(str(cup) for cup in self.cups(start=1))[1:]

    def move(self):

        # determine the length of the cups
        length = len(self)

        # remove the three cups immediately clockwise of current cup
        picked_up = [self.get_successor(self.current)]
        picked_up.append(self.get_successor(picked_up[-1]))
        picked_up.append(self.get_successor(picked_up[-1]))
        self.set_successor(self.current, self.get_successor(picked_up[-1]))

        # determine position of destination cup
        destination = (self.current - 2) % length + 1
        while destination in picked_up:
            destination = (destination - 2) % length + 1

        # insert the removed cups at the destination
        self.set_successor(picked_up[-1], self.get_successor(destination))
        self.set_successor(destination, picked_up[0])

        # select a new current cup
        self.current = self.get_successor(self.current)

        return self

    def moves(self, count):

        for _ in range(count):
            self.move()

        return self

def labels(cups, moves):

    """
    >>> labels(389125467, 0)
    25467389
    >>> labels(389125467, 1)
    54673289
    >>> labels(389125467, 2)
    32546789
    >>> labels(389125467, 3)
    34672589
    >>> labels(389125467, 4)
    32584679
    >>> labels(389125467, 5)
    36792584
    >>> labels(389125467, 6)
    93672584
    >>> labels(389125467, 7)
    92583674
    >>> labels(389125467, 8)
    58392674
    >>> labels(389125467, 9)
    83926574
    >>> labels(389125467, 10)
    92658374
    >>> labels(389125467, 100)
    67384529

    >>> labels(952316487, 100)
    25398647
    """

    cups = [int(cup) for cup in str(cups)]
    return int(Cups(cups).moves(moves).order)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
