#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# BAD Solution

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def is_cat_prime(primes):
    for i in range(len(primes)-1):
        for j in range(i+1, len(primes)):
            if not is_prime( int( str(primes[i]) + str(primes[j]) ) ) or not is_prime( int( str(primes[j]) + str(primes[i]) ) ):
                return False
    return True

def next_prime(start = 2, stop = -1):
    if stop == -1:
        xbool = lambda x, y: True
    else:
        xbool = lambda x, y: x < y
    while xbool(start, stop) :
        if is_prime(start): yield start
        start += 1

primes = [3]
for i in xrange(7,6000):
    if is_prime(i):
        primes.append(i)

cat_sol = []
for i in xrange(len(primes)-1):
    for j in xrange(i+1, len(primes)):
        if is_cat_prime( [ primes[i], primes[j] ] ):
            print 0,
            for k in xrange(j+1, len(primes)):
                if is_cat_prime( [ primes[i], primes[j], primes[k] ] ):
                    cat_sol.append( [ primes[i], primes[j], primes[k] ] )

for k in ([6000,7000],[7000,8500]):
    for i in xrange(k[0], k[1]):
        if is_prime(i):
            primes.append(i)

    cat_sol2 = []
    final = len(primes)-1
    for i in cat_sol:
        xbool = True
        j = 0
        while primes[final] > primes[j] and xbool:
            if i[-1] < primes[j] and is_cat_prime( i + [primes[j]] ):
                i.append(primes[j])
                cat_sol2.append(i)
                xbool = False
            j += 1

    cat_sol = cat_sol2


print min(map(sum,cat_sol))

# Time ~ 10 min