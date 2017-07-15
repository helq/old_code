#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def is_perm(num1, num2):
    num1, num2 = str(num1), str(num2)
    if not len(num1) == len(num2):
        return False
    for i in num1:
        j = 0
        xbool = True
        while xbool and j < len(num2):
            if i == num2[j]:
                num2 = num2[:j] + num2[j+1:]
                xbool = False
            j += 1
        if xbool:
            return False
    return True

for i in xrange(10000,1000000000):
    xbool = True
    j = 2
    while xbool and j < 8:
        if not is_perm(i, j * i):
            xbool = False
        j += 1
    if xbool and j == 8:
        print i
        break
