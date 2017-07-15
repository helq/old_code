#!/usr/bin/env python2
# -*- coding: utf-8 -*-

triangle = map( lambda x: [ int(i) for i in x.split(" ") ], open("067-triangle.txt", "Ur").readlines() )

# OR, but the first is most beautiful that second ;)
#triangle = map( lambda x: eval("[" + x.replace(" ",",")[:-1].replace("08","8").replace("09","9") + "]"), open("067-triangle.txt", "Ur").readlines() )

size_triangle = range(len(triangle)-1, -1, -1)

for i in size_triangle:
    for j in range(i):
        if triangle[i][j] > triangle[i][j + 1]:
            triangle[i - 1][j] += triangle[i][j]
        else:
            triangle[i - 1][j] += triangle[i][j + 1]

print triangle[0][0]

