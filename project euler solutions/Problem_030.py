#!/usr/bin/env python2
# -*- coding: utf-8 -*-

sum_powers = 0
for i in range(2,1000000):
    n = 0
    j = 1
    while j < 1000000:
        n += (( i % (j * 10) )/j)**5
        j *= 10
    if n == i:
        sum_powers += i

print sum_powers