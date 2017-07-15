#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Use shedskin:
# see: http://en.wikipedia.org/wiki/Farey_Sequence

def farey( n ):
    a, b, c, d = 0, 1, 1, n
    i = 1
    while c <= n:
        k = int((n + b)/d)
        a, b, c, d = c, d, k*c - a, k*d - b
        i += 1
    return i

print farey(1000000) - 2