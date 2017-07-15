#!/usr/bin/env python2
# -*- coding: utf-8 -*-

num = 20
numero = num
while True:
    for i in range(1,num + 1):
        if numero % i != 0:
            break
        else:
            if i == num:
                print numero
                exit()
    numero += 1
