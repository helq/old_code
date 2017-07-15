#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt
maximum_ratio = 1/(2+sqrt(2))

def num_solutions(num):
    num_sol = 0
    for i in range(1, int( num * maximum_ratio ) + 1 ):
        for j in range(i, (num - 1)/2 ):
            if i**2 + j**2 == (num - i - j)**2:
                num_sol += 1
    return num_sol

num_with_max_sol = 0
max_num_sol = 0
for i in range(100,1001):
    num_sol = num_solutions(i)
    if max_num_sol < num_sol:
        max_num_sol = num_sol
        num_with_max_sol = i

print num_with_max_sol


""" Se demora ligeramente más O_o
num_with_max_sol = 0
max_num_sol = 0
for num in range(100,1001):
    num_sol = 0
    for i in range(1, int( num * maximum_ratio ) + 1 ):
        for j in range(i, (num - 1)/2 ):
            if i**2 + j**2 == (num - i - j)**2:
                num_sol += 1

    if max_num_sol < num_sol:
        max_num_sol = num_sol
        num_with_max_sol = num

print num_with_max_sol
"""