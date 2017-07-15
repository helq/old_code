#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# NOTA: Usar ShedSkin para convertir el código a C++ y así ahorrarse tiempo.
# Tiempo aproximado de ejcución: 3~4 min

from math import ceil

def sum_factors(x):
    sums = 0
    for i in range( 1, int( ceil( x/2.0 + 1 ) ) ):
        if x % i == 0:
            sums += i
    return sums

abundant_nums = []
for i in range( 28124 ):
    if i < sum_factors( i ):
        abundant_nums.append(i)

sum_abundant_nums = range(28124)
for i in range( len(abundant_nums) ):
    for j in range( i , len(abundant_nums) ):
        a = abundant_nums[i] + abundant_nums[j]
        if a < 28124:
            try:
                sum_abundant_nums.remove(a)
            except:
                None

result = reduce(lambda x, y: x + y , sum_abundant_nums)
print result