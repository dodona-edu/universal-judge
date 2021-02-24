# hexagonal coordinate systems:
# https://www.redblobgames.com/grids/hexagons/
# https://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/AV0405/MARTIN/Hex.pdf

class HexagonalTile:

    """
    >>> tile = HexagonalTile(0, 0)
    >>> tile
    HexagonalTile(0, 0)
    >>> print(tile)
    (0, 0)
    >>> tile.neighbour('e').neighbour('se').neighbour('ne').neighbour('e')
    HexagonalTile(6, 0)
    >>> tile.neighbour('nw').neighbour('w').neighbour('sw').neighbour('e').neighbour('e')
    HexagonalTile(0, 0)
    >>> tile.path('esenee')
    HexagonalTile(6, 0)
    >>> tile.path('nwwswee')
    HexagonalTile(0, 0)
    """

    directions = {
        'e': (2, 0),
        'se': (1, 1),
        'sw': (-1, 1),
        'w': (-2, 0),
        'nw': (-1, -1),
        'ne': (1, -1)
    }

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return f'HexagonalTile({self.x}, {self.y})'

    def __str__(self):
        return f'({self.x}, {self.y})'

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def neighbour(self, direction):
        assert direction in HexagonalTile.directions, 'unknown direction'
        dx, dy = HexagonalTile.directions[direction]
        return HexagonalTile(self.x + dx, self.y + dy)

    @property
    def neighbours(self):
        for dx, dy in HexagonalTile.directions.values():
            yield HexagonalTile(self.x + dx, self.y + dy)

    def path(self, directions):
        tile = self
        direction = ''
        for step in directions:
            direction += step
            if direction in HexagonalTile.directions:
                tile = tile.neighbour(direction)
                direction = ''
        return tile

def black_tiles(filename, days):

    """
    >>> black_tiles('tiles.txt', 0)
    10
    >>> black_tiles('tiles.txt', 1)
    15
    >>> black_tiles('tiles.txt', 2)
    12
    >>> black_tiles('tiles.txt', 3)
    25
    >>> black_tiles('tiles.txt', 4)
    14
    >>> black_tiles('tiles.txt', 5)
    23
    >>> black_tiles('tiles.txt', 6)
    28
    >>> black_tiles('tiles.txt', 7)
    41
    >>> black_tiles('tiles.txt', 8)
    37
    >>> black_tiles('tiles.txt', 9)
    49
    >>> black_tiles('tiles.txt', 10)
    37
    >>> black_tiles('tiles.txt', 20)
    132
    >>> black_tiles('tiles.txt', 30)
    259
    >>> black_tiles('tiles.txt', 40)
    406
    >>> black_tiles('tiles.txt', 50)
    566
    >>> black_tiles('tiles.txt', 60)
    788
    >>> black_tiles('tiles.txt', 70)
    1106
    >>> black_tiles('tiles.txt', 80)
    1373
    >>> black_tiles('tiles.txt', 90)
    1844
    >>> black_tiles('tiles.txt', 100)
    2208

    >>> black_tiles('adventofcode.input.txt', 100)
    3804
    """

    # determine original configuration of black tiles
    configuration = set()
    reference_tile = HexagonalTile(0, 0)
    with open(filename) as paths:
        for path in paths:
            tile = reference_tile.path(path.rstrip('\n'))
            if tile in configuration:
                configuration.remove(tile)
            else:
                configuration.add(tile)

    # flip tiles day by day
    for _ in range(days):

        # determine number of black neighbour tiles
        neighbours = {}
        for tile in configuration:
            for neighbour in tile.neighbours:
                neighbours[neighbour] = neighbours.get(neighbour, 0) + 1

        # determine new configuration
        new_configuration = set()
        for tile in configuration:
            if 1 <= neighbours.get(tile, 0) <= 2:
                new_configuration.add(tile)
        for tile, count in neighbours.items():
            if tile not in configuration and count == 2:
                new_configuration.add(tile)

        # set the new configuration
        configuration = new_configuration

    return len(configuration)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
