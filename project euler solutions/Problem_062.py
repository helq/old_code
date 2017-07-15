#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def is_perm(num1, num2):
    num1, num2 = str(num1), str(num2)
    if not len(num1) == len(num2):
        return False
    for i in num1:
        j = 0
        xbool = True
        while xbool and j < len(num2):
            if i == num2[j]:
                num2 = num2[:j] + num2[j+1:]
                xbool = False
            j += 1
        if xbool:
            return False
    return True

def is_cube_perm5(nums, cubes):
    if len(nums) == 5: return True
    i = 0
    while i < len(cubes):
        if is_perm(nums[-1], cubes[i]):
            return is_cube_perm5(nums + [cubes[i]], cubes[i+1:])
        i += 1
    return False

cubes = [n**3 for n in range(4641,10000)]

for i in range(len(cubes) - 4):
    if is_cube_perm5( [cubes[i]], cubes[i+1:]):
        print cubes[i]

