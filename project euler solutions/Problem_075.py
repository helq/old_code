#!/usr/bin/env python2
# -*- coding: utf-8 -*-

f = eval( file("075-total.txt","r").readline() )

total = [0]*1500001

for i in f:
    for j in range(1,1500000/i+1):
        total[j*i]+=1

total_trian = 0
for i in total:
    total_trian += i==1

print total_trian

#total2 = []
#for i in range(len(total)):
    #if total[i] == 1: total2.append(i)
