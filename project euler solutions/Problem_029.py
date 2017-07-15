#!/usr/bin/env python2
# -*- coding: utf-8 -*-

eval_pot = []
for i in range(2,101):
    for j in range(2,101):
        a = i ** j
        try:
            n = eval_pot.index(a)
        except ValueError:
            eval_pot.append(a)

print len(eval_pot)
