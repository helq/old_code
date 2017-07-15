#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def fact(num):
    if num == 0: return 1
    return num * fact(num - 1)

def prod(final, start = 0):
    if final == start: return 1
    return final * prod(final - 1, start)

count = 0
for i in range(1,101):
    for j in range(1,i-1):
        count += prod(i,j)/fact(i-j) >= 1000000

print count