#!/usr/bin/env python2
# -*- coding: utf-8 -*-

max_sum_digit = 0
for a in range(90,100):
    for b in range(90,100):
        aux = sum( int(n) for n in str(a**b))
        if max_sum_digit < aux :
            max_sum_digit = aux

print max_sum_digit


""" OR most memory
print max( sum( int(n) for n in str(a**b) ) for a in xrange(90,100) for b in xrange(90,100) )
"""