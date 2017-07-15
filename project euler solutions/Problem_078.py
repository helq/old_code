#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# see: http://mathworld.wolfram.com/PartitionFunctionP.html (equation 11)

solve = [1,1]

def p(n):
    def sign(x):
        return (-1) if x%2==0 else (1)
    
    if   n < 0:  return 0
    elif n < len(solve):
        return solve[n]
    else:
        i = 0
        for k in range(1,n+1):
            i += sign(k)*( p(n-k*(3*k-1)/2) + p(n-k*(3*k+1)/2) )
        return i

i = 1
while True:
    i += 1
    solve_i = p(i)%1000000
    solve.append(solve_i)
    if solve_i==0:
        break

print i

