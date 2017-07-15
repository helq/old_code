#!/usr/bin/env python2
# -*- coding: utf-8 -*-

o = file("022-names.txt","r")
a = o.readline()
letter = False

names = []

ini_let = 0
i = 0
while( i < len( a ) ):
    if a[i] == '"':
        if letter:
            names.append(a[ ini_let : i ])
        else:
            ini_let = i + 1
        letter = not letter
    i += 1

names.sort()

sum_names = 0
i = 0

while( i < len(names) ):
    sum_letters = 0
    for j in names[i]:
        sum_letters += ord(j) - 64
    sum_names += sum_letters * (i + 1)
    i += 1

print sum_names