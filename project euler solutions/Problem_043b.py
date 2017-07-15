#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# by ilan

primes = [2, 3, 5, 7, 11, 13, 17]

def getPerms(a):
   if len(a)==1:
      yield a
   else:
      for i in range(len(a)):
         this = a[i]
         rest = a[:i] + a[i+1:]
         for p in getPerms(rest):
            yield [this] + p

s = 0

for k in getPerms(range(10)):
    OK=True
    for j in range(7):
        if (100*k[j+1]+10*k[j+2]+k[j+3]) % primes[j]:
            OK=False
            break
    if OK:
        s += int(''.join(map(str,k)))

print s