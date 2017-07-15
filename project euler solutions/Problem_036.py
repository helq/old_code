#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def is_palindrom(num):
    num = str(num)
    for i in range(len(num)/2):
        if not num[i] == num[-i - 1]:
            return False
    return True

sum_palindrom_numbers = 0

for i in range(1000000):
    if is_palindrom(i) and is_palindrom(bin(i)[2:]):
        sum_palindrom_numbers += i

print sum_palindrom_numbers