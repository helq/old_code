#!/usr/bin/env python2
# -*- coding: utf-8 -*-

sum_equal_factorial_digits = 0
for num in range(3, 50000):
    sum_num = 0
    for i in range(len(str(num))):
        aux = 1
        for j in range(1, (num%10**(i+1))/(10**i) + 1 ): aux *= j
        sum_num += aux
    if num == sum_num:
        sum_equal_factorial_digits += num

print sum_equal_factorial_digits