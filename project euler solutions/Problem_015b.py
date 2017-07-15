#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Very very slow

def rutas(n):
	def f(n,j):
		if j==1: return n
		else:
			a=0
			for i in range(n,0,-1):
				a+=f(i,j-1)
		return a
	return f(n+1,n-1)*2

size_grid = int(raw_input("Tamaño de la cuadrícula: "))

print rutas(size_grid)