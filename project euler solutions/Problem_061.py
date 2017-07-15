#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from math import sqrt

is_exactly = lambda x: x - int(x) == 0

is_trian = lambda x: is_exactly( sqrt( .25 + 2*x ) - .5 )
is_squa = lambda x: is_exactly( sqrt( x ) )
is_pent = lambda x: is_exactly( (.5 + sqrt(.25 + 6*x) )/3 )
is_hexa = lambda x: is_exactly( ( 1 + sqrt(1 + 8*x) )/4 )
is_hept = lambda x: is_exactly( ( sqrt(2.25 + 10*x) )/5 + .3 )
#is_octa = lambda x: is_exactly( ( 1 + sqrt(1 + 3*x) )/3 )

#trian = lambda n: n*(n + 1)/2
#squa = lambda n: n*(n)
#pent = lambda n: n*(3*n - 1)/2
#hexa = lambda n: n*(2*n - 1)
#hept = lambda n: n*(5*n - 3)/2
octa = lambda n: n*(3*n - 2)

def is_prop_cyclic(array, poly):
    if len(array) == 6:
        if array[-1]%100 == array[0]/100:
            yield True, array
        else:
            yield False,
    lim = (array[-1] % 100)*100
    for i in range( lim + 10 , lim + 100 ):
        j = 0
        while j < len(poly):
            if poly[j](i):
                for is_cyclic in is_prop_cyclic(array + [i], poly[:j] + poly[j+1:]):
                    yield is_cyclic
            j += 1
    yield False,

poly = (is_trian, is_squa, is_pent, is_hexa, is_hept)
for i in range(19, 59):
    octa_i = octa(i)
    if str(octa_i)[-2] != "0":
        for prop in is_prop_cyclic([octa_i], poly):
            if prop[0]:
                print prop[1], sum(prop[1])
