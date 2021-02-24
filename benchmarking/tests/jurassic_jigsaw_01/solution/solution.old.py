import itertools

class Tile:

    """
    >>> tile = Tile(1234, '#..##.#.#.#.####')
    >>> tile
    Tile(1234, '#..##.#.#.#.####')
    >>> print(tile)
    #..#
    #.#.
    #.#.
    ####
    >>> tile.border_top()
    '#..#'
    >>> tile.border_bottom()
    '####'
    >>> print(tile.rotate())
    ####
    #...
    ###.
    #..#
    >>> tile.border_left()
    '####'
    >>> tile.border_right()
    '#..#'
    >>> print(tile.rotate(counterclockwise=True))
    #..#
    #.#.
    #.#.
    ####
    >>> print(tile.flip())
    ####
    #.#.
    #.#.
    #..#
    >>> print(tile.flip(vertical=True))
    ####
    .#.#
    .#.#
    #..#
    """

    def __init__(self, id, grid):
        self.id = id
        if isinstance(grid, str):
            size = round(len(grid) ** 0.5)
            grid = [grid[row * size:(row + 1) * size] for row in range(size)]
        self.grid = grid

    def __str__(self):
        return '\n'.join(''.join(row) for row in self.grid)

    def __repr__(self):
        grid = ''.join(''.join(row) for row in self.grid)
        return f'Tile({self.id}, {grid!r})'

    @property
    def size(self):
        return len(self.grid[0])

    def border_left(self):
        return ''.join(row[0] for row in self.grid)

    def border_right(self):
        return ''.join(row[-1] for row in self.grid)

    def border_top(self):
        return ''.join(self.grid[0])

    def border_bottom(self):
        return ''.join(self.grid[-1])

    def rotate(self, counterclockwise=False):
        self.grid = [
            [
                self.grid[c][self.size - r - 1]
                if counterclockwise else
                self.grid[self.size - c - 1][r]
                for c in range(self.size)
            ]
            for r in range(self.size)
        ]
        return self

    def flip(self, vertical=False):
        if vertical:
            self.grid = [row[::-1] for row in self.grid]
        else:
            self.grid = [
                self.grid[self.size - index - 1]
                for index in range(self.size)
            ]
        return self

    def arrangements(self):
        for _ in range(2):
            for _ in range(4):
                yield self.rotate()
            self.flip()

def solver(tiles):

    size = round(len(tiles) ** 0.5)
    grid = [[None] * size for _ in range(size)]

    def place(remaining_tiles):

        if not remaining_tiles:
            return True

        # print('current grid')
        # print('\n'.join(' '.join('####' if cell is None else str(cell.id) for cell in row)for row in grid))

        # find best position:
        #   - criterium 1: maximum number of occupied neighbouring cells
        #   - criterium 2: maximum number of free neighbouring cells
        best_position = None
        best_values = None
        for r, c in itertools.product(tuple(range(size)), repeat=2):
            if grid[r][c] is None:
                occupied, free = 0, 0
                for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                    if 0 <= r + dr < size and 0 <= c + dc < size:
                        if grid[r + dr][c + dc] is None:
                            free += 1
                        else:
                            occupied += 1
                if best_position is None or (occupied, free) > best_values:
                    best_position = (r, c)
                    best_values = (occupied, free)

        # try to place all remaining tiles at the best position
        r, c = best_position
        checks = [
            (-1, 0, Tile.border_top, Tile.border_bottom),
            (1, 0, Tile.border_bottom, Tile.border_top),
            (0, -1, Tile.border_left, Tile.border_right),
            (0, 1, Tile.border_right, Tile.border_left),
        ]
        for index, tile in enumerate(remaining_tiles):

            # check if tile fits at the given position in any of the arrangements
            for tile in tile.arrangements():
                if all(
                    not (0 <= r + dr < size) or
                    not (0 <= c + dc < size) or
                    grid[r + dr][c + dc] is None or
                    own_border(tile) == neighbour_border(grid[r + dr][c + dc])
                    for (dr, dc, own_border, neighbour_border) in checks
                ):
                    grid[r][c] = tile
                    if place(remaining_tiles[:index] + remaining_tiles[index + 1:]):
                        return True
                    grid[r][c] = None

        # no solution found
        return False

    if place(tiles):
        return grid

    return None

def corners(filename):

    """
    >>> corners('tiles.txt')
    20899048083289
    """

    """
    >>> corners('adventofcode.input.txt')
    4006801655873
    """

    # read tiles from the puzzle
    tiles, grid, tile_id = [], [], None
    with open(filename, 'r', encoding='utf-8') as lines:
        for line in lines:
            line = line.rstrip('\n')
            if line.startswith('Tile'):
                tile_id = int(line[5:-1])
            elif line:
                grid.append(list(line))
            else:
                tiles.append(Tile(tile_id, grid))
                grid = []
        else:
            tiles.append(Tile(tile_id, grid))

    solution = solver(tiles)
    if solution:
        return (
            solution[0][0].id *
            solution[0][-1].id *
            solution[-1][0].id *
            solution[-1][-1].id
        )

if __name__ == '__main__':
    import doctest
    doctest.testmod()
