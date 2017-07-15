#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def len_cycle(dividend,divisor):
    dividends = []
    while True:
        if dividend > divisor:
            try:
                n = dividends.index(dividend)
                return len(dividends) - n
            except ValueError:
                dividends.append(dividend)
                dividend %= divisor
        elif dividend == 0:
            return 0
        else:
            dividend *= 10

max_cycle = 0
max_long_cycle = 0

for i in range(1,1000):
    len_cycle_i = len_cycle(1,i)
    if max_long_cycle < len_cycle_i:
        max_long_cycle = len_cycle_i
        max_cycle = i

print max_cycle