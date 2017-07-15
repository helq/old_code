#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

primes = []
for i in range(1, 6000):
    if is_prime(i):
        primes.append(i)

def is_square(num):
    n = sqrt(num)
    return n - int(n) == 0

def is_odd_composite(num):
    i = 0
    while primes[i] < num:
        if is_square( (num - primes[i]) / 2.0 ):
            return True
        i += 1
    return False

xbool = True
i = 19
while xbool:
    if not is_prime(i) and not is_odd_composite(i):
        xbool = False
        print i
    i += 2