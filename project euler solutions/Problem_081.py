#!/usr/bin/env python2
# -*- coding: utf-8 -*-

matrix = [eval( "["+x[:-2]+"]" ) for x in open("081-matrix.txt","r").readlines()]

#matrix = [[131, 673, 234, 103, 18 ],
          #[201, 96,  342, 965, 150],
          #[630, 803, 746, 422, 111],
          #[537, 699, 497, 121, 956],
          #[805, 732, 524, 37,  331]]

def minSumMatrix(matrix):
    n = len(matrix)
    matrix2 = [[0]*n for i in range(n)]

    def minSumPos(i,j):
        if i == 0 and j == 0:
            return 0
        elif i == 0:
            return matrix2[0][j-1]
        elif j == 0:
            return matrix2[i-1][0]
        if matrix2[i-1][j] > matrix2[i][j-1]:
            return matrix2[i][j-1]
        else:
            return matrix2[i-1][j]

    for i in range(n):
        for j in range(n):
            matrix2[i][j] = minSumPos(i,j) + matrix[i][j]
    return matrix2[-1][-1]

print minSumMatrix(matrix)
