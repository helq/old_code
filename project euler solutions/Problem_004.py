#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import floor

def capicua(numero):
    numero_str = str(numero)
    numero_list = []
    n = 0
    for i in numero_str:
        numero_list.append(i)
        n += 1
    for i in range(int(floor(n/2))):
        if numero_list[i] != numero_list[n-i-1]:
            return False
    return True

digitos = int(raw_input("Introduce el número de dígitos generadores del capicua: "))
referencia = "1"
for i in range(digitos - 1):
    referencia += "0"
referencia_2 = int(referencia)
referencia += "0"
referencia = int(referencia) - 1

i = referencia
while i > referencia_2:
    j = referencia
    while j > referencia_2:
        if capicua(i * j):
            print "Los mayores números de tres cifras que generan un número capicua son: %d y %d\ny el número capicua generado por ellos es: %d" % (j,i,i * j)
            exit()
        j -= 1
    i -= 1
