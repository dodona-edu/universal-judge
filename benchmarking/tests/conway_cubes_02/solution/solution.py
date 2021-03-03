import itertools

class Cube:

    """
    >>> cube = Cube(0, 0, 0)
    >>> print(cube)
    (0, 0, 0)
    """

    def __init__(self, *coordinates):
        self.coordinates = coordinates
        self.dimension = len(coordinates)

    def __str__(self):
        return str(self.coordinates)

    def __repr__(self):
        return str(self)

    @property
    def neighbours(self):
        for direction in itertools.product((-1, 0, 1), repeat=self.dimension):
            if direction != (0, ) * self.dimension:
                yield Cube(*[c + dc for c, dc in zip(self.coordinates, direction)])

    def __hash__(self):
        return hash(self.coordinates)

    def __eq__(self, other):
        return self.coordinates == other.coordinates

class Region:

    def __init__(self, filename, dimension=3):
        with open(filename, 'r', encoding='utf8') as region:
            self.active = {
                Cube(x, y, *([0] * (dimension - 2)))
                for x, row in enumerate(region)
                for y, cube in enumerate(row.rstrip('\n'))
                if cube == '#'
            }

    @property
    def active_cubes(self):
        return len(self.active)

    def __iter__(self):
        return self

    def __next__(self):

        # collect number of active neighbouring cells
        neighbours = {}
        for cube in self.active:
            for neighbour in cube.neighbours:
                neighbours[neighbour] = neighbours.get(neighbour, 0) + 1

        # cycle region
        self.active = (
            {cube for cube in self.active if neighbours.get(cube, 0) in {2, 3}} |
            {cube for cube, count in neighbours.items() if count == 3 and cube not in self.active}
        )

        return self

def active_cubes(filename, cycles=0):

    """
    >>> active_cubes('cubes.txt', 0)
    5
    >>> active_cubes('cubes.txt', 1)
    29
    >>> active_cubes('cubes.txt', 2)
    60
    >>> active_cubes('cubes.txt', 3)
    320
    >>> active_cubes('cubes.txt', 4)
    188
    >>> active_cubes('cubes.txt', 5)
    1056
    >>> active_cubes('cubes.txt', 6)
    848

    >>> active_cubes('adventofcode.input.txt', 6)
    2552
    """

    # initialize the region
    region = Region(filename, dimension=4)

    # cycle through the next states of the region
    for _ in range(cycles):
        next(region)

    # return the number of active cubes in the region
    return region.active_cubes

if __name__ == '__main__':
    import doctest
    doctest.testmod()
