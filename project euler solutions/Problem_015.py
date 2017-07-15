#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def crear_matriz(filas, columnas, contenido = 0):
    matriz = []
    for i in range(filas):
        matriz.append([contenido] * columnas)
    return matriz

size_grid = int(raw_input("Tamaño de la cuadrícula: "))

matriz_sums = crear_matriz(size_grid - 1, size_grid + 1)

for i in range(size_grid + 1):
    matriz_sums[0][i] = i + 1
for j in range(size_grid - 2):
    matriz_sums[j + 1][0] = 1

for j in range(1, size_grid - 1):
    for i in range(1, size_grid + 1):
        matriz_sums[j][i] = matriz_sums[j][i - 1] + matriz_sums[j - 1][i]


sum_final = 0
for i in range(size_grid + 1):
    sum_final += matriz_sums[-1][i]
print "Cantidad de rutas posibles %d" % sum_final
