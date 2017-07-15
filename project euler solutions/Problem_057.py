#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def fraction_root():
    a = 1
    b = 2
    while True:
        yield a + b, b
        a += b * 2
        a, b = b, a

next_fract = fraction_root()
total_fract = 0
for i in range(1001):
    fract = next_fract.next()
    total_fract += len(str(fract[0])) > len(str(fract[1]))

print total_fract