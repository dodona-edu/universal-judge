# hexagonal coordinate systems:
# https://www.redblobgames.com/grids/hexagons/
# https://homepages.inf.ed.ac.uk/rbf/CVonline/LOCAL_COPIES/AV0405/MARTIN/Hex.pdf

class HexagonalTile:

    """
    >>> tile = HexagonalTile(0, 0, 0)
    >>> tile
    HexagonalTile(0, 0, 0)
    >>> print(tile)
    (0, 0, 0)
    >>> tile.neighbour('e').neighbour('se').neighbour('ne').neighbour('e')
    HexagonalTile(3, 3, 0)
    >>> tile.neighbour('nw').neighbour('w').neighbour('sw').neighbour('e').neighbour('e')
    HexagonalTile(0, 0, 0)
    >>> tile.path('esenee')
    HexagonalTile(3, 3, 0)
    >>> tile.path('nwwswee')
    HexagonalTile(0, 0, 0)
    """

    directions = {
        'e': (1, 1, 0),
        'se': (1, 0, -1),
        'sw': (0, -1, -1),
        'w': (-1, -1, 0),
        'nw': (-1, 0, 1),
        'ne': (0, 1, 1)
    }

    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __repr__(self):
        return f'HexagonalTile({self.x}, {self.y}, {self.z})'

    def __str__(self):
        return f'({self.x}, {self.y}, {self.z})'

    def __hash__(self):
        return hash((self.x, self.y, self.z))

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y and self.z == other.z

    def neighbour(self, direction):
        assert direction in HexagonalTile.directions, 'unknown direction'
        dx, dy, dz = HexagonalTile.directions[direction]
        return HexagonalTile(self.x + dx, self.y + dy, self.z + dz)

    def neighbours(self):
        for dx, dy, dz in HexagonalTile.directions.values():
            yield HexagonalTile(self.x + dx, self.y + dy, self.z + dz)

    def path(self, directions):
        tile = self
        direction = ''
        for step in directions:
            direction += step
            if direction in HexagonalTile.directions:
                tile = tile.neighbour(direction)
                direction = ''
        return tile

def black_tiles(filename):

    """
    >>> black_tiles('tiles.txt')
    10
    >>> black_tiles('adventofcode.input.txt')
    317
    """

    # determine configuration of black tiles
    configuration = set()
    reference_tile = HexagonalTile(0, 0, 0)
    with open(filename) as paths:
        for path in paths:
            tile = reference_tile.path(path.rstrip('\n'))
            if tile in configuration:
                configuration.remove(tile)
            else:
                configuration.add(tile)

    return len(configuration)

if __name__ == '__main__':
    import doctest
    doctest.testmod()
