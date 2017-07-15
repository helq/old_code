#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def is_pal(num): #Is Palindrome?
    num = str(num)
    for i in range(len(num)):
        if num[i] != num[-i - 1]:
            return False
    return True

def pal_gen(num):
    i = 0
    while (not is_pal(num) or 0 == i) and i < 50 :
        num += int( str(num)[-1::-1] )
        i += 1
    if i == 50: i = 0
    return i

total_lychrel = 0
for i in range(10,10000):
    total_lychrel += pal_gen(i) == 0

print total_lychrel