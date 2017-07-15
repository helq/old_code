#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from decimal import *

is_exacly = lambda x: x == int(x)
is_sqrt = lambda x: is_exacly(x**.5)

def frac2sqrt(num, a_n = [], min_div = []):
    if len(a_n) == 0 or num < 0:
        if is_sqrt(num) or num < 0: return (num, [])
        else:
            aux = int(num**.5)
            a_n = [aux]
            min_div = [(aux, 1)]
    new_div = (num - (min_div[-1][0])**2)/min_div[-1][1]
    new_a_n = int((num**.5 + min_div[-1][0] ) / new_div )
    new_min = new_a_n * new_div - min_div[-1][0]
    if min_div[1:].count( (new_min, new_div) ) > 0:
        return a_n[0], a_n[1:]
    else:
        return frac2sqrt(num, a_n + [new_a_n], min_div + [(new_min, new_div)])


def min_sol_Diophan(D):
    if is_sqrt(D): return False,
    i = 0
    a, b = 0, 1
    a_b = frac2sqrt(D)
    a_b = [a_b[0]] + 2*a_b[1]
    while True:
        #print b
        a, b = b, b*a_b[i] + a
        c = int(((b**2 - 1)/Decimal(D))**Decimal(".5"))
        if b**2 - D * (c**2) == 1 :
            return b, int(c)
        i += 1

getcontext().prec = 50

max_sol = 0,
max_D = 0
for i in range(2,1000):
    sol_D_i = min_sol_Diophan(i)
    if sol_D_i[0] > max_sol[0]:
        max_sol, max_D = sol_D_i, i

print "D: ", max_D, "  (x, y) :", max_sol