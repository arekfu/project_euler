#!/usr/bin/env python3

DICT_PATHS = {}

def n_paths(x, y):
    if (x, y) in DICT_PATHS:
        return DICT_PATHS[(x, y)]

    if x == 0 or y == 0:
        paths = 1
    else:
        paths = n_paths(x - 1, y) + n_paths(x, y - 1)
    DICT_PATHS[(x, y)] = paths
    return paths

print(n_paths(20, 20))
