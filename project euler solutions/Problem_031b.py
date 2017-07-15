#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# "Solution by arn.zar"

def nway( total, coins):
    if not coins: return 0
    c, coins = coins[0], coins[1:]
    count = 0
    if total % c == 0: count += 1
    for amount in xrange( 0, total, c):
        count += nway(total - amount, coins)
    return count

# main
print nway( 200, (1,2,5,10,20,50,100,200))

#OR
"""
def gencount( coins): #NOTE: an amount must be positive
  if len( coins) == 0: return lambda i0: 0
  if len( coins) == 1: return lambda i0: int( i0 % coins[0] == 0)

  coins = sorted(coins, reverse=True)
  code = 'lambda i0: sum(( 1'
  for i, c in enumerate(coins[:-1]):
    code += ' for i%d in xrange( i%d, -1, -%d) ' %\
            ( i+1, i, c)
  if coins[-1] != 1: code += ' if i%d %% %d == 0' % ( i+1, coins[-1] )
  code += ' ))'
  return eval( code)
print gencount( ( 1, 2, 5, 10, 20, 50, 100, 200 ))( 200) # 73682 0.028 seconds
"""