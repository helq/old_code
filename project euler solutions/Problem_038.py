#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def is_product_pandigital(num):
    cat_product = ''
    i = 1
    while len(cat_product) < 10:
        cat_product += str( num * i )
        if len(cat_product) == 9:
            cat_product += '0'
            for i in range(len(cat_product) - 1):
                for j in cat_product[i+1:] :
                    if j == cat_product[i]:
                        return False, cat_product[:-1]
            return True, cat_product[:-1]
        i += 1
    return False, cat_product

max_product_pandigital = (0, 0)
for i in range(10000):
    a = is_product_pandigital(i)
    if a[0] and a[1] > max_product_pandigital[1]:
        max_product_pandigital = (i, a[1])

print max_product_pandigital