#!/usr/bin/env python2
# -*- coding: utf-8 -*-

total = 0
for i in range(1,10):
    for j in range(1,25):
        if len(str(i**j)) == j:
            print i, j, i**j
            total += 1

print total