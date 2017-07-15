#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def is_permutation(num1, num2):
    for i in str(num1):
        perm = False
        for j in str(num2):
            if i == j:
                perm = True
        if not perm:
            return False
    return True

primes = []
for i in range(1488, 3340):
    if is_prime(i):
        primes.append(i)

for i in primes:
    if is_prime(i + 3330) and is_prime(i+6660) and is_permutation(i, i+3330) and is_permutation(i, i+6660):
        print "".join(map(str,(i,i+3330,i+6660)))

