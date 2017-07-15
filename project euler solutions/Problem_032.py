#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def is_pandigital(num): #String
    if len(num) == 9:
        num += '0'
        for i in range(len(num) - 1):
            for j in num[i+1:] :
                if j == num[i]:
                    return False
        return True
    else:
        return False

pan_prod = []

for i in range(1,2000):
    for j in range(1,2000):
        n = i * j
        if is_pandigital(str(i) + str(j) + str(n)):
            try:
                a = pan_prod.index(n)
            except ValueError:
                pan_prod.append(n)

print sum(pan_prod)
