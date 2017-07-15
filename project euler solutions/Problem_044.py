#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt
import sys

pent = [n*(3*n-1)/2 for n in range(1,2300)]

def is_pent(x): # "by emandres" thx ;)
    f = (.5 + sqrt(.25+6*x))/3
    if f - int(f) == 0:
        return True
    else:
        return False

for i in range( 1, len(pent) + 1 ):
    for j in range( i + 1, len(pent) ):
        if is_pent( pent[i] + pent[j] ) and is_pent( pent[j] - pent[i] ):
            print pent[j] - pent[i]
            sys.exit()




""" Use shedskin
import sys

pentagonal_nums = [n*(3*n-1)/2 for n in range(1,2300)]

for i in range(1, len(pentagonal_nums)+1):
    for j in range(i+1, len(pentagonal_nums)+1):
        try:
            u = pentagonal_nums.index(pentagonal_nums[i]+pentagonal_nums[j])
            try:
                u = pentagonal_nums.index( abs(pentagonal_nums[i] - pentagonal_nums[j]) )
                print pentagonal_nums[i], pentagonal_nums[j], pentagonal_nums[u]
                sys.exit()
            except:
                None
        except:
            None
"""