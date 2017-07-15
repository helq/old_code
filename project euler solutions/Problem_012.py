#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def number_divisors(number):
    total_divisors = 0
    for i in range(1,int(number / 2) + 1):
        if not number % i:
            total_divisors += 1
    return total_divisors + 1

number = 150
i = 0
triangle_number = 0

while True:
    i += 1
    triangle_number += i
    if number <= number_divisors(triangle_number):
        print(triangle_number, number_divisors(triangle_number))
        break
