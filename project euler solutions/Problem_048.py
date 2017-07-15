#!/usr/bin/env python2
# -*- coding: utf-8 -*-

eval_serie = 0
for i in range(1,1001):
    eval_serie += i ** i

print str(eval_serie)[-10:]
