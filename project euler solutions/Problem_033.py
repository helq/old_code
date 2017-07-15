#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def is_equal(num1, num2):
    if num1 == num2:
        return False
    elif num1/10 == num2%10 and num2/10 != 0 and float(num1)/num2 == float(num1%10)/(num2/10) < 1.0:
        return True
    elif num1%10 == num2/10 and num2%10 != 0 and float(num1)/num2 == float(num1/10)/(num2%10) < 1.0:
        return True
    else: return False

ans1, ans2 = 1, 1

for i in range(10,100):
    for j in range(10,100):
        if is_equal(i, j):
            ans1 *= i
            ans2 *= j

print ans2/ans1