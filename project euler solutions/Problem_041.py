#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def minus(a,b):
    j = 0
    for k in range(len(a)):
        if a[j] > a[k] and b < a[k]:
            j = k
    return j

def aum(a):
    i = 0
    xbool = True
    while( xbool ):
        i -= 1
        if( a[i] > a[i-1] ):
            xbool = False
            j = minus(a[i:],a[i-1]) + i
            a[j], a[i-1] = a[i-1], a[j]
    b = a[i:]
    b.sort()
    return a[:i] + b

max_pandigital_prime = 0
for i in range(8,10):
    pandigital_array = range(1,i)
    const = pandigital_array[:]
    const.reverse()
    while const != pandigital_array:
        cat = ''
        for j in pandigital_array:
            cat += str(j)
        cat = int(cat)
        if cat > max_pandigital_prime and is_prime(cat):
            max_pandigital_prime = cat
        pandigital_array = aum(pandigital_array)

print max_pandigital_prime