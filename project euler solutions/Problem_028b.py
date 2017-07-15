#!/usr/bin/env python2
# -*- coding: utf-8 -*-

sum_diagonal = 1
position = 1
for i in range(2,1001,2):
    for j in range(4):
        position += i
        sum_diagonal += position

print sum_diagonal