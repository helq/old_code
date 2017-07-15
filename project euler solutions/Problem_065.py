#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def sum_cont_frac(frac, nume = 1, deno = 0):
    if deno == 0:
        if len(frac) == 1: return frac[0],
        deno = frac[-1]
        return sum_cont_frac( frac[:-1], nume, deno )
    if len(frac) == 1: return frac[0] * deno + nume, deno
    return sum_cont_frac( frac[:-1], deno, frac[-1]*deno + nume )

list_e = [2,1] + [ n*i + (not i) for n in range(2,67,2) for i in (1,0,0) ]

print sum( int(i) for i in str(sum_cont_frac( list_e[:100] )[0]) )