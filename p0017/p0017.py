#!/usr/bin/env python3

NUMBERS = {
    0: '',
    1: 'one',
    2: 'two',
    3: 'three',
    4: 'four',
    5: 'five',
    6: 'six',
    7: 'seven',
    8: 'eight',
    9: 'nine',
    10: 'ten',
    11: 'eleven',
    12: 'twelve',
    13: 'thirteen',
    14: 'fourteen',
    15: 'fifteen',
    16: 'sixteen',
    17: 'seventeen',
    18: 'eighteen',
    19: 'nineteen',
    20: 'twenty',
    30: 'thirty',
    40: 'forty',
    50: 'fifty',
    60: 'sixty',
    70: 'seventy',
    80: 'eighty',
    90: 'ninety',
    1000: 'one thousand'
    }

def n_to_letters(n):
    if n in NUMBERS:
        return NUMBERS[n]
    elif n<100:
        d = n % 10
        return ' '.join([NUMBERS[n - d], NUMBERS[d]])
    else:
        h = n // 100
        cc = n % 100
        if cc>0:
            return NUMBERS[h] + ' hundred and ' + n_to_letters(cc)
        else:
            return NUMBERS[h] + ' hundred'

def len_skip_spaces(s):
    return len([x for x in s if x != ' '])

tot = 0
for i in range(1,1001):
    tot += len_skip_spaces(n_to_letters(i))

print(tot)
