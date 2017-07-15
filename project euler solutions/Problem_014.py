#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def size_chain(num):
    i = 0
    while True:
        i += 1
        if num == 1:
            return i
        elif num % 2 == 0:
            num /=2
        else:
            num = num*3 + 1

num = 0
size_chain_num = 0
for i in range(500001,1000000):
    n = size_chain(i)
    if n > size_chain_num:
        size_chain_num = n
        num = i

print num