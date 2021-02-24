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

    >>> tile = Tile(0, '.####...#####..#...###..#####..#..#.#.####..#.#..#.#...#.###...#.##.##..#.#.##.###.#.##.##.#####..##.###.####..#.####.##...#.#..##.##...#..#..###.##.#..#.#..#..##.#.#...###.##.....#...###.#...#.####.#.#....##.#..#.#.##...#..#....#..#...####..#.##...###..#.#####..#....#.##.#.#####....#.....##.##.###.....#.##..#.#...#...###..####....##..#.##...#.##.#.#.###...##.###.#..####...##..#...#.###...#.##...#.######..###.###.#######..#####...##.#..#..#.#######.####.#..##.########..#..##.#.#####..#.#...##..#....#....##..#.#########..###...#.....#..##...###.###..###....##.#...##.##.#')
    >>> tile.sea_monsters
    (2, 273)
    """

    # find all relative positions of the sea monster
    monster = [
        '                  # ',
        '#    ##    ##    ###',
        ' #  #  #  #  #  #   '
    ]
    monster_rows, monster_cols = len(monster), len(monster[0])
    monster = {
        (r, c)
        for r, row in enumerate(monster)
        for c, cell in enumerate(row)
        if cell == '#'
    }

    def __init__(self, id, grid):
        self.id = id
        if isinstance(grid, str):
            size = round(len(grid) ** 0.5)
            grid = [
                list(grid[row * size:(row + 1) * size])
                for row in range(size)
            ]
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

    def borders(self, all=False):
        for border in (
            self.border_top(),
            self.border_right(),
            self.border_bottom(),
            self.border_left()
        ):
            yield border
            if all:
                yield border[::-1]

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
        for _ in range(4):
            yield self.rotate()
            yield self.flip()
            self.flip()
            yield self.flip(vertical=True)
            self.flip(vertical=True)

    @property
    def monsters(self):
        for r in range(self.size  - self.monster_rows + 1):
            for c in range(self.size - self.monster_cols + 1):
                if all(self.grid[r + dr][c + dc] == '#' for dr, dc in Tile.monster):
                    yield (r, c)

    @property
    def sea_monsters(self):

        # spot all monsters
        monsters = list(self.monsters)

        # show all monsters by the letter O
        for r, c in monsters:
            for dr, dc in Tile.monster:
                self.grid[r + dr][c + dc] = 'O'

        # count the number of hashes (#)
        count = 0
        for row in self.grid:
            count += row.count('#')

        # hide all monsters again behind hashes (#)
        for r, c in monsters:
            for dr, dc in Tile.monster:
                self.grid[r + dr][c + dc] = '#'

        return len(monsters), count

class Puzzle:

    checks = [
        (-1, 0, Tile.border_top, Tile.border_bottom),
        (1, 0, Tile.border_bottom, Tile.border_top),
        (0, -1, Tile.border_left, Tile.border_right),
        (0, 1, Tile.border_right, Tile.border_left),
    ]

    def __init__(self, filename):

        # read tiles from the file
        self.tiles, grid, tile_id = {}, [], None
        with open(filename, 'r', encoding='utf-8') as lines:
            for line in lines:
                line = line.rstrip('\n')
                if line.startswith('Tile'):
                    tile_id = int(line[5:-1])
                elif line:
                    grid.append(list(line))
                else:
                    self.tiles[tile_id] = Tile(tile_id, grid)
                    grid = []
            else:
                self.tiles[tile_id] = Tile(tile_id, grid)

        # initialize grid
        self.size = round(len(self.tiles) ** 0.5)
        self.grid = [[None] * self.size for _ in range(self.size)]
        self.placed_tiles = set()
        self.solve()

    def place_tile(self, r, c, tile):
        self.grid[r][c] = tile.id
        self.placed_tiles.add(tile.id)

    def remove_tile(self, r, c):
        self.placed_tiles.remove(self.grid[r][c])
        self.grid[r][c] = None

    @property
    def solution(self):

        return self.grid

    def solve(self):

        # map each possible border to set of tiles with that border
        borders = {}
        for tile_id, tile in self.tiles.items():
            for border in tile.borders(all=True):
                if border not in borders:
                    borders[border] = set()
                borders[border].add(tile_id)

        # map tile to number of fitting neighbours
        # NOTE: tiles will be attempted with increasing number of possible fits
        neighbours = {}
        for tiles in borders.values():
            for tile1, tile2 in itertools.combinations(tiles, 2):
                neighbours[tile1] = neighbours.get(tile1, 0) + 1
                neighbours[tile2] = neighbours.get(tile2, 0) + 1

        # traverse the grid positions in diagonal order, inside out
        diagonal_order = sorted(
            itertools.product(range(self.size), repeat=2),
            key=lambda coord: (sum(coord), max(coord))
        )

        def place_remaining_tiles(position=0):

            # recursion ends if all tiles have been placed
            remaining_tiles = set(self.tiles) - self.placed_tiles
            if not remaining_tiles:
                return True

            # print('current grid')
            # print('\n'.join(' '.join('####' if cell is None else str(cell.id) for cell in row)for row in grid))

            # determine next position to place a tile
            # r, c = position // self.size, position % self.size
            # r, c = find_next_position()
            r, c = diagonal_order[position]

            # reduce remaining tiles based on neighbouring borders
            for dr, dc, _, border in Puzzle.checks:
                if (
                    0 <= r + dr < self.size and
                    0 <= c + dc < self.size and
                    self.grid[r + dr][c + dc] is not None
                ):
                    remaining_tiles &= borders[border(self.tiles[self.grid[r + dr][c + dc]])]

            # try to place all remaining tiles at the next position
            for index, tile_id in enumerate(sorted(remaining_tiles, key=lambda tile: neighbours[tile])):

                # check if tile fits at the given position in any of the arrangements
                for tile in self.tiles[tile_id].arrangements():
                    if all(
                        not (0 <= r + dr < self.size) or
                        not (0 <= c + dc < self.size) or
                        self.grid[r + dr][c + dc] is None or
                        own_border(tile) == neighbour_border(self.tiles[self.grid[r + dr][c + dc]])
                        for (dr, dc, own_border, neighbour_border) in Puzzle.checks
                    ):
                        self.place_tile(r, c, tile)
                        if place_remaining_tiles(position + 1):
                            return True
                        self.remove_tile(r, c)

            # no solution found
            return False

        if place_remaining_tiles():
            return self.grid

        return None

def corners(filename):

    """
    >>> corners('tiles.txt')
    20899048083289
    >>> corners('adventofcode.input.txt')
    4006801655873
    """

    # fetch the solution of the puzzle (grid of tile IDs)
    solution = Puzzle(filename).solution

    # compute the product of the four corner tiles
    return solution[0][0] * solution[0][-1] * solution[-1][0] * solution[-1][-1]

def water_roughness(filename):

    """
    >>> water_roughness('tiles.txt')
    273
    >>> water_roughness('adventofcode.input.txt')
    1838
    """

    puzzle = Puzzle(filename)
    solution = puzzle.solution

    # compose grid out of solution
    grid = []
    for solution_row in solution:
        for row in zip(*[str(puzzle.tiles[tile]).split('\n')[1:-1] for tile in solution_row]):
            grid.append(''.join(chunk[1:-1] for chunk in row))

    # spot monsters in any possible arrangement
    tile = Tile(0, grid)
    for tile in tile.arrangements():
        monsters, hashes = tile.sea_monsters
        if monsters:
            return hashes

if __name__ == '__main__':
    import doctest
    doctest.testmod()
