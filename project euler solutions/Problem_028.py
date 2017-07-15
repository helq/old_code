#!/usr/bin/env python2
# -*- coding: utf-8 -*-

sum_diagonal = 1
position = 1
for i in range(2,1001,2):
    sum_diagonal += 4 * position + 10 * i
    position += 4 * i

print sum_diagonal