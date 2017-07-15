#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    a = abs(int(a))
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def is_circular_prime(num):
    num = str(num)
    for i in range(len(num)):
        if not is_prime(int( num[i:] + num[0:i] )):
            return False
    return True

sum_prime_circular = 0
for i in range(2,1000000):
    if is_circular_prime(i):
        sum_prime_circular += 1

print sum_prime_circular