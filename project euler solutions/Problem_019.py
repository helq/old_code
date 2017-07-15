#!/usr/bin/env python2
# -*- coding: utf-8 -*-

days_on_month = {
                1: 31, 2: 28, 3: 31,
                4: 30, 5: 31, 6: 30,
                7: 31, 8: 31, 9: 30,
                10: 31, 11: 30, 12: 31
                }

day = {
      0: "Sun",
      1: "Mon",
      2: "Tue",
      3: "Wed",
      4: "Thu",
      5: "Fri",
      6: "Sat"
      }

def date(day, month, year): # only 1900-2399 ;)
    num_day = (year - 1900)*365
    num_day += (year - 1900)/4
    if ( year % 4 == 0 ) and ( year % 400 != 0 ) and month > 2 :
        num_day += 1
    for i in range(1,month):
        num_day += days_on_month[i]
    num_day += day
    return num_day

num_sunday = 0

for i in range(1901,2001):
     for j in range(1,13):
         if date(1,j,i) % 7 == 0:
             num_sunday += 1

print num_sunday