#!/usr/bin/env python2
# -*- coding: utf-8 -*-

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
max_primes = []
sum_primes = 0
while sum_primes < 1000000:
    sum_primes += nxprime.next()
    if 958577 < sum_primes < 1000000:
        max_primes.append(sum_primes)

primes = []
for i in range(2, 400):
    if is_prime(i):
        primes.append(i)

max_prime = []
for i in max_primes:
    j = 0
    while i > 985577:
        i -= primes[j]
        j += 1
        if is_prime(i):
            max_prime.append(i)
            i = 0

print max(max_prime)