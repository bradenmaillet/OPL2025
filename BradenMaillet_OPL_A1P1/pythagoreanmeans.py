'''
Code sourced from:
    https://rosettacode.org/wiki/Averages/Pythagorean_means#Python
'''


from operator import mul
from functools import reduce


def amean(num):
    ### Calculate the arithmetic mean.
    return sum(num) / len(num)


def gmean(num):
    ### Calculate the geometric mean.
    return reduce(mul, num, 1)**(1 / len(num))


def hmean(num):
    ### Calculate the harmonic mean.
    return len(num) / sum(1 / n for n in num)


numbers = range(1, 11)  # 1..10
a, g, h = amean(numbers), gmean(numbers), hmean(numbers)
### Call each function for calculations.
print(a, g, h)

assert a >= g >= h
### Conditional tool to confirm that the arithmetic mean >= geometric mean >= harmonic mean.
