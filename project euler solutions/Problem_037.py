#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def is_truncable(num):
    if not is_prime(num): return False
    num = str(num)
    for i in range(len(num) - 1):
        if not is_prime(int(num[i+1:])) or not is_prime(int(num[:-i - 1])):
            return False
    return True

total = 0
i = 10
sum_truncables = 0
while total < 11:
    if is_prime(i): print i, is_truncable(i)
    if is_truncable(i):
        sum_truncables += i
        total += 1
    i += 1

print sum_truncables

