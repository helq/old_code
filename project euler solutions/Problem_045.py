#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

#trian = lambda n: n*(n + 1)/2
#pent = lambda n: n*(3*n - 1)/2
hexa = lambda n: n*(2*n - 1)

def is_trian(x):
    f = sqrt( .25 + 2*x ) - .5
    return f - int(f) == 0

def is_pent(x):
    f = (.5 + sqrt(.25 + 6*x) )/3
    return f - int(f) == 0

# main
equal = False
i = 285
while not equal:
    i += 1
    equal = is_trian(hexa(i)) and is_pent(hexa(i))

print hexa(i)