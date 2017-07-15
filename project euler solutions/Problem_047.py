#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def fact_primes(num):
    fact = []
    i = 1
    while i <= num :
        if num % i == 0 and is_prime(i):
            num /= i
            if len(fact) != 0 and fact[-1] % i == 0:
                fact[-1] *= i
            else:
                fact.append(i)
        else:
            i += 1
    return fact

def is_repeat_elem(array):
    for i in range(len(array)):
        for j in range(i+1,len(array)):
            if array[i] == array[j]:
                return True
    return False

i = 1
xbool = True
while xbool:
    fact1 = fact_primes(i)
    if len(fact1) == 4:
        fact2 = fact_primes(i + 1)
        if len(fact2) == 4:
            fact3 = fact_primes(i + 2)
            if len(fact3) == 4:
                fact4 = fact_primes(i + 3)
                if len(fact4) == 4:
                    if not is_repeat_elem(fact1 + fact2 + fact3 + fact4):
                        xbool = False
                        print i
    i += 1