#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# "My solution" :D

coins = (200, 100, 50, 20, 10, 5, 2, 1)

def value(a):
    sum_coins = 0
    for i in range(len(a)):
        sum_coins += a[i]*coins[i]
    return sum_coins

def other_com(a):
    xbool = True
    i = -2
    while xbool:
        if a[i] == 0:
            i -= 1
        else:
            a[i] -= 1
            for j in range(len(a)+i+1,len(a)):
                a[j] = (200 - value(a[:j])) / coins[j]
            xbool = False

combination = [1] + 7 * [0]
num_comb = 1

xbool = True
while xbool:
    other_com(combination)
    if combination[-1] == 200:
        xbool = False
    num_comb += 1

print num_comb
