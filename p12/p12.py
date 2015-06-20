#!/usr/bin/env python3

import math
from collections import Counter
import operator
import functools

DICT_FACTORS = dict()

def factorize(n):
    if n in DICT_FACTORS:
        return DICT_FACTORS[n]

    cnt = Counter()
    sqrtn = int(math.sqrt(n)) + 1
    for i in range(2, sqrtn + 1):
        if n % i == 0:
            cnt[i] += 1
            cnt += factorize(n // i)
            break
    else:
        cnt[n] += 1
    DICT_FACTORS[n] = cnt
    return cnt

def n_divisors(n):
    factors = factorize(n)
    n_div = functools.reduce(operator.mul, (val+1 for val in factors.values()), 1)
    return n_div

def triangular_number(n):
    return n*(n+1)//2

i=100
while True:
    n = triangular_number(i)
    n_div = n_divisors(n)
    if n_div>500:
        print(n)
        break
    print('{}, {}: {}'.format(n, i, n_div))
    i = i + 1
