class Deck:

    """
    >>> deck = Deck([3, 2, 10, 6, 8, 5, 9, 4, 7, 1])
    >>> print(deck)
    3, 2, 10, 6, 8, 5, 9, 4, 7, 1
    >>> deck.score
    306
    """

    def __init__(self, cards):

        # always take a copy of the cards
        self.cards = list(cards)

    def __len__(self):

        return len(self.cards)

    def __str__(self):

        return ', '.join(str(card) for card in self.cards)

    @property
    def isempty(self):

        return not len(self)

    def draw(self):

        return self.cards.pop(0)

    def add(self, card):

        self.cards.append(card)
        return self

    @property
    def score(self):

        length = len(self)
        return sum(
            value * (length - index)
            for index, value in enumerate(self.cards)
        )

    def __hash__(self):

        return hash(tuple(self.cards))

    def __eq__(self, other):

        return self.cards == other.cards

class Combat:

    """
    >>> game = Combat([9, 2, 6, 3, 1], [5, 8, 4, 7, 10])
    >>> print(game)
    Player 1's deck: 9, 2, 6, 3, 1
    Player 2's deck: 5, 8, 4, 7, 10
    >>> print(next(game))
    Player 1's deck: 2, 6, 3, 1, 9, 5
    Player 2's deck: 8, 4, 7, 10
    >>> print(next(game))
    Player 1's deck: 6, 3, 1, 9, 5
    Player 2's deck: 4, 7, 10, 8, 2
    >>> print(next(game))
    Player 1's deck: 3, 1, 9, 5, 6, 4
    Player 2's deck: 7, 10, 8, 2
    >>> print(next(game))
    Player 1's deck: 1, 9, 5, 6, 4
    Player 2's deck: 10, 8, 2, 7, 3
    """

    def __init__(self, player1, player2, history=None):

        self.player1 = Deck(player1)
        self.player2 = Deck(player2)

    def __str__(self):

        return (
            f"Player 1's deck: {self.player1}\n" +
            f"Player 2's deck: {self.player2}"
        )

    @property
    def winner(self):

        # check if the first player wins
        if self.player2.isempty:
            return 1

        # check if the second player wins
        if self.player1.isempty:
            return 2

        # the game hasn't been won yet
        return 0

    @property
    def iswon(self):

        return bool(self.winner)

    @property
    def score(self):

        if self.winner == 1:
            return self.player1.score

        if self.winner == 2:
            return self.player2.score

        raise AssertionError('game has not ended yet')

    def __iter__(self):

        return self

    def __next__(self):

        # check if the game has finished
        if self.iswon:
            raise StopIteration()

        # each player draws the top card of his deck
        card1 = self.player1.draw()
        card2 = self.player2.draw()

        # determine the winner by playing a recursive game if needed
        winner = 1 if card1 > card2 else 2

        # winner puts both cards at bottom of his deck (his own card first)
        if winner == 1:
            self.player1.add(card1).add(card2)
        else:
            self.player2.add(card2).add(card1)

        return self

    def play(self):

        # this limitation is for the only for generating random games
        remaining_rounds = 1000

        # play the game until finished
        while remaining_rounds and not self.iswon:
            remaining_rounds -= 1
            next(self)

        return self

def read_decks(filename):

    deck1, deck2 = [], []
    deck = deck1
    with open(filename, 'r', encoding='utf-8') as lines:
        for line in lines:
            line = line.rstrip('\n')
            if not line:
                deck = deck2
            elif not line.startswith('P'):
                deck.append(int(line))

    return deck1, deck2

def winning_score(filename):

    """
    >>> winning_score('decks.txt')
    306
    >>> winning_score('adventofcode.input.txt')
    33473
    """

    # 1. read the two decks from file
    # 2. play the game until finished
    # 3. return the winning player's score
    return Combat(*read_decks(filename)).play().score

if __name__ == '__main__':
    import doctest
    doctest.testmod()
