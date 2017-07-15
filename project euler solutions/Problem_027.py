#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    a = abs(int(a))
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def num_primes(a,b):
    i = 0
    while True:
        if not is_prime( i*(i + a) + b ):
            break
        else:
            i += 1
    return i

max_num_primes = 0
for i in range(-999,1000):
    for j in range(-999,1000):
        n = num_primes(i,j)
        if n > max_num_primes:
            max_num_primes = n
            max_mult = i * j

print max_mult