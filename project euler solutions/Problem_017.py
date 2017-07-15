#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""
one
two
three
four
five
six
seven
eight
nine
ten
eleven
twelve
thirteen
fourteen
fifteen
sixteen
seventeen
eighteen
nineteen
twenty
twenty-one
thirty
thirty-one
forty
fifty
sixty
seventy
eighty
ninety
hundred
one hundred and one
two hundred
two hundred forty-three
three hundred
four hundred
five hundred
six hundred
seven hundred
eight
nine hundred
thousand
"""

# hundred, thousand Special

units = {1: 3,
         2: 3,
         3: 5,
         4: 4,
         5: 4,
         6: 3,
         7: 5,
         8: 5,
         9: 4}

tens = {2: 6,
        3: 6,
        4: 5,
        5: 5,
        6: 5,
        7: 7,
        8: 6,
        9: 6}

special = { 10: 3,
            11: 6,
            12: 6,
            13: 8,
            14: 8,
            15: 7,
            16: 7,
            17: 9,
            18: 8,
            19: 8}

def size_number(num):
    if num == 0:
        return 0
    elif num < 10:
        return units[num]
    elif 10 <= num < 20:
        return special[num]
    elif 20 <= num < 100:
        return tens[num/10] + size_number( num % 10 )
    elif 100 <= num < 1000:
        size = units[num/100] + 7
        if not num % 100 == 0:
            size += 3
        return size + size_number( num % 100 )
    elif num == 1000:
        return 11

sum_nums = 0
for i in range(1,1001):
    sum_nums += size_number(i)

print sum_nums