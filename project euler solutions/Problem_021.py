#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import ceil

def sum_factors(x):
    sums = 0
    for i in range( 1, int( ceil( x/2.0 + 1 ) ) ):
        if x % i == 0:
            sums += i
    return sums

sum_amic = 0

for i in range(1,10001):
    a = sum_factors(i)
    if i < a and sum_factors(a) == i :
        sum_amic += a + i

print sum_amic