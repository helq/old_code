#!/usr/bin/env python2
# -*- coding: utf-8 -*-

i = 1
a, b = 1, 1
while len(str(a)) < 1000:
    a, b = b, a + b
    i += 1

print i