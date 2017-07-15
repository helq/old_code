#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

i = 0
position = 1
sum_primes = 0
while sum_primes/float(2*i + 1) > 0.1 or sum_primes == 0:
    i += 2
    for j in range(4):
        position += i
        sum_primes += is_prime(position)

print i + 1