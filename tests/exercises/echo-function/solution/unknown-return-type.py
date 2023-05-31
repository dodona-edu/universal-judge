from dataclasses import dataclass


@dataclass
class Coord:
    x: int
    y: int


def echo(content):
    return Coord(5, 6)
