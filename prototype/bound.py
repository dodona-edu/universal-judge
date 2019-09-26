import math
import random

import numpy
from scipy.stats import linregress, boxcox


def lin_confidence(xs, ys):
    res = linregress(xs, ys)
    return res[2]


def next_order(a, direction, estep=0.001):
    return a + estep * direction


def trend(x, y, cutoff_parameter=0.1):
    res = linregress(x, y)
    if abs(res[2]) > cutoff_parameter:
        return numpy.sign(res[0]), res[4]
    return 0, res[4]


# Only usable for y = a_1 * x ^ b_1 + a_2 * x ^ b_2 + a_3 * x ^ b_3 + ...
def guess_ratio(xs, ys):
    b = 0

    def calculate_ratios(xs, ys, b):
        return [y / (x ** b) for x, y in zip(xs, ys)]

    direction, error = trend(xs, calculate_ratios(xs, ys, b))
    while direction > 0:
        b = next_order(b, direction)
        direction, error = trend(xs, calculate_ratios(xs, ys, b))
    return b, 1 - error


def guess_ratio_log(xs, ys):
    b = 0

    def calculate_ratios(xs, ys, b):
        return [y / (x ** b * math.log(x)) for x, y in zip(xs, ys)]

    direction, error = trend(xs, calculate_ratios(xs, ys, b))
    while direction > 0:
        b = next_order(b, direction)
        direction, error = trend(xs, calculate_ratios(xs, ys, b))
    return b, 1 - error


# Only usable for y = a * x ^ b + c
def guess_difference(xs, ys):
    b = 0

    def calculate_differences(xs, ys, b):
        return [(x ** b) - y for x, y in zip(xs, ys)]

    diffs = calculate_differences(xs, ys, b)
    while min(diffs) != diffs[0]:
        b = next_order(b, 1)
        diffs = calculate_differences(xs, ys, b)
    return next_order(b, -1), None


# Only usable for y = a * x ^ b
def power(xs, ys):
    logxs = numpy.log(xs)
    logys = numpy.log(ys)

    res = linregress(logxs, logys)
    return res[0], 1 - res[4]


# Only usable for y = a * x ^ b
def box_cox(xs, ys):
    res = boxcox(ys, alpha=.95)
    interval = (1 / res[2][1], 1 / res[2][0])

    return interval, 0.95


def difference(xs, ys):
    d = 0
    while trend(xs, ys)[0] > 0:
        ys = [(y1 - y2) / (x1 - x2) for x1, x2, y1, y2 in zip(xs, xs[1:], ys, ys[1:])]
        xs = xs[:-1]
        d += 1
    return d, None


if __name__ == '__main__':
    fun = lambda x: 10 * (x ** 2) + 5 * x + 100 + random.randint(0, 10)
    print(f'Guess Ratio: {guess_ratio(list(range(20, 200, 5)), list(map(fun, range(20, 200, 5))))}')
    print(f'Guess Difference: {guess_difference(list(range(20, 400, 5)), list(map(fun, range(20, 400, 5))))}')
    print(f'Power: {power(list(range(20, 200, 5)), list(map(fun, range(20, 200, 5))))}')
    print(f'Box-Cox: {box_cox(list(range(20, 400, 5)), list(map(fun, range(20, 400, 5))))}')
    print(f'Difference: {difference(list(range(20, 200, 5)), list(map(fun, range(20, 200, 5))))}')
