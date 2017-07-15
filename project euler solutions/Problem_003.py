#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import ceil, sqrt

def es_primo(numero_ori):
    if numero_ori == 2:
        return True
    i = 2
    limite = ceil(sqrt(numero_ori)) + 1
    while i < limite:
        if numero_ori % i == 0:
            return False
        i += 1
    return True

numero_original = int(raw_input("Este programa busca los números primos que dividen a: "))
numero = int(ceil(sqrt(numero_original)) + 1)

for i in range(2,numero):
    if es_primo(i):
        if numero_original % i == 0:
            print i,
