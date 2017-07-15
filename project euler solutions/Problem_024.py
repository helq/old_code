#!/usr/bin/env python2
# -*- coding: utf-8 -*-

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

a = list(range(10))
for i in range(999999):
    a = aum(a)

b = ""
for i in a:
    b += str(i)

print b

"""
while(True):
    print a, i
    try:
        a = aum(a)
    except:
        print i
        break
    i += 1
"""
