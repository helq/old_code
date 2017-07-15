#!/usr/bin/env python2
# -*- coding: utf-8 -*-

numero_1, numero_2, suma_fibonacci = 1, 1, 0

while numero_2 < 4000000:
    if numero_2 % 2 == 0:
        suma_fibonacci += numero_2
    numero_1, numero_2 = numero_2, numero_1 + numero_2

print suma_fibonacci
