#!/usr/bin/env python2
# -*- coding: utf-8 -*-

primes = (2,3,5,7,11,13,17)

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

def array_to_num(array):
    num = ''
    for i in array:
        num += str(i)
    return int(num)

def is_unusual_div(a):
    for i in range(7):
        if int(str(a[i+1]) + str(a[i+2]) + str(a[i+3])) % primes[i]:
            return False
    return True

pandigital_nums = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
final = [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
sum_pandigital_nums = 0
while pandigital_nums != final:
    pandigital_nums = aum(pandigital_nums)
    if is_unusual_div(pandigital_nums):
        sum_pandigital_nums += array_to_num(pandigital_nums)

print sum_pandigital_nums