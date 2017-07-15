#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import ceil, sqrt

def es_primo(numero_ori):
    if numero_ori == 2:
        return True
    i = 2
    numero = ceil(sqrt(numero_ori)) + 1
    while i < numero:
        if numero_ori % i == 0:
            return False
        i += 1
    return True

j = 0
k = int(raw_input("> ")) + 1
for i in range(2,k):
    if es_primo(i):
        j += i

print j
