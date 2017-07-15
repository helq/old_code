#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

def is_prime(a):
    if a == 1: return False
    for i in range( 2, int(sqrt(a)) + 1 ):
        if a % i == 0:
            return False
    return True

def next_prime(start = 2, stop = -1):
    if stop == -1:
        xbool = lambda x, y: True
    else:
        xbool = lambda x, y: x < y
    while xbool(start, stop) :
        if is_prime(start): yield start
        start += 1

def num_repeat(num):
    repeats = []
    for i in range(len(num)-1):
        for j in range(i+1, len(num)):
            if num[i] == num[j]:
                try:
                    repeats.index(num[i])
                except:
                    repeats.append(num[i])
    return repeats

def replace_digits(num, before, after):
    for i in range(len(num)):
        if num[i] == before:
            num = num[:i] + after + num[i+1:]
    return num

def is_change8(num, array_change):
    if not int(min(array_change)) <= 2:
        return False
    for i in array_change:
        if int(i) <= 2:
            composites = 0
            for j in range(int(i),10):
                if not is_prime(int(replace_digits(num, i, str(j)))):
                    composites += 1
            if composites <= 2 - int(i):
                return True
    return False

for i in next_prime(56003):
    num = str(i)
    num_repeat_i = num_repeat(num)
    if len(num_repeat_i) > 0 and is_change8(num, num_repeat_i):
        break

print i
