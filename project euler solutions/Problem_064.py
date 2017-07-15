#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

is_exactly = lambda x: x - int(x) == 0
is_squa = lambda x: is_exactly( sqrt( x ) )

def frac2sqrt(num, a_n = [], min_div = []):
    if len(a_n) == 0 or num < 0:
        if is_squa(num) or num < 0: return (num, [])
        else:
            aux = int(sqrt(num))
            a_n = [aux]
            min_div = [(aux, 1)]
    new_div = (num - (min_div[-1][0])**2)/min_div[-1][1]
    new_a_n = int((sqrt(num) + min_div[-1][0] ) / new_div )
    new_min = new_a_n * new_div - min_div[-1][0]
    if min_div[1:].count( (new_min, new_div) ) > 0:
        return a_n[0], a_n[1:]
    else:
        return frac2sqrt(num, a_n + [new_a_n], min_div + [(new_min, new_div)])

total_odd_frac = 0
for i in range(10000):
    frac_sqrt_i = frac2sqrt(i)
    if len(frac_sqrt_i[-1]) % 2 != 0:
        total_odd_frac += 1

print total_odd_frac
