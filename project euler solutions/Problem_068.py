#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def minimum(c):
    j, minn = 0, 10
    for k in range(5):
        if minn > c[k][0]:
            j, minn = k, c[k][0]
    return c[j:] + c[:j]

def getPerms(a):
   if len(a)==1:
      yield a
   else:
      for i in range(len(a)):
         this = a[i]
         rest = a[:i] + a[i+1:]
         for p in getPerms(rest):
            yield [this] + p

def solSet(xs,ys):
    xs, ys = 2*xs[:], ys[:]
    n_set = ys[0] + sum(xs[0:2])
    for i in range(1,len(ys)):
        if n_set != ys[i] + sum(xs[i:i+2]): return False
    return True

solSets = []
for i in getPerms(range(1,11)):
    xs = i[:5]
    ys = i[5:]
    if solSet(xs, ys):
        zs = []
        xs *= 2
        for i in range(5):
            zs += [[ys[i]] + xs[i:i+2]]
        solSets += [zs]

minSolSets = []
for i in solSets:
    minSolSets += [minimum(i)]

l = max(minSolSets)

print l
print "".join([str(x) for x in reduce(lambda x,y: x+y,l)])