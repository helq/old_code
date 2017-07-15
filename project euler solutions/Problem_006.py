#!/usr/bin/env python2
# -*- coding: utf-8 -*-

suma, suma_cuadrados = 0, 0
for i in range(1,101):
    suma_cuadrados += i ** 2
    suma += i

suma **= 2
print suma - suma_cuadrados
