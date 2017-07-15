#!/usr/bin/env python2
# -*- coding: utf-8 -*-

total_product = 1
i = 1
n = 1
total_len = 0
while i < 1000000:
    total_len += len(str(n))
    if total_len >= i:
        total_product *= (n/int(10**(total_len-i)))%10
        i *= 10
    n += 1

print total_product
