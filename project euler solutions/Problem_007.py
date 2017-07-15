#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import ceil, sqrt

def es_primo(numero_ori):
    i = 2
    numero = ceil(sqrt(numero_ori)) + 1
    while i < numero:
        if numero_ori % i == 0:
            return False
        i += 1
    return True

numero = int(raw_input("Este programa busca el número primo número (sin contar al 1): "))

i, j = 0, 1
while True:
    if es_primo(j):
        i += 1
    if i == numero:
        print j
        exit()
    j += 1
