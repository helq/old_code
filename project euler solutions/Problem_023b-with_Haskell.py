#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# run Time: few seconds with runHaskell ~ 12 s

import subprocess as sub

p = sub.Popen(["runhaskell", "Problem_023.hs"], stdout=sub.PIPE)

aNums, b = p.communicate()
aNums = eval(aNums)

lenNums = 28123

noAbundantNums = [True for i in range(lenNums+1)]

lenAbun = len( aNums )

for i in xrange(lenAbun):
    for j in xrange(i, lenAbun):
        noAbun = aNums[i] + aNums[j]
        if noAbun <= lenNums:
            noAbundantNums[noAbun] = False
        else: break

tot = 0
for i in xrange(lenNums+1):
    if noAbundantNums[i]:
        tot += i

print tot # 4179871
