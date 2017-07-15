#!/usr/bin/env python2
# -*- coding: utf-8 -*-

n = 0
for i in range(int(raw_input("Este pequeño programa es para sumar todos los múltiplos de 3 y 5 que se\nencuentran por debajo de el número: "))):
    if i % 3 == 0 or i % 5 == 0:
        n += i

print n
