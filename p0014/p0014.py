#!/usr/bin/env python3

def next_collatz(n):
    if n == 1:
        return None
    elif n % 2 == 0:
        return n // 2
    else:
        return 3 * n + 1

LEN_COLLATZ = {1: 1}

def collatz(n):
    i = n
    l = []

    while i is not None:
        l += [ i ]
        i = next_collatz(i)

    return l

def len_collatz(n):
    if n in LEN_COLLATZ:
        return LEN_COLLATZ[n]

    this_len = 1 + len_collatz(next_collatz(n))
    LEN_COLLATZ[n] = this_len
    return this_len

cur_max = 0
cur_best = 0
for i in range(1, 1000001):
    l = len_collatz(i)
    if l > cur_max:
        cur_max = l
        cur_best = i

print('{}: {}'.format(cur_best, cur_max))
    
