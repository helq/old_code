#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Complete - No eficient

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def next_prime():
    i = 2
    while True:
        if is_prime(i): yield i
        i += 1

nxprime = next_prime()
array_sum_primes = []
sum_primes = 0
while sum_primes < 1000000:
    sum_primes += nxprime.next()
    if is_prime(sum_primes): max_prime = sum_primes
    array_sum_primes.append(sum_primes)

if max_prime != max(array_sum_primes):
    primes = []
    for i in range(2, 1000000 - max_prime):
        if is_prime(i):
            primes.append(i)

    array_max_prime = []
    for i in array_sum_primes:
        j = 0
        while max_prime < i < 1000000:
            i -= primes[j]
            j += 1
            if is_prime(i):
                array_max_prime.append(i)
                i = 0

    print max(array_max_prime), array_max_prime
else:
    print max_prime