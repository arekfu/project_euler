#!/usr/bin/env python3

def sum_digits(n):
    return sum(int(d) for d in str(n))

print(sum_digits(2**1000))
