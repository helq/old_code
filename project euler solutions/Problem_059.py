#!/usr/bin/env python2
# -*- coding: utf-8 -*-

def decrypt(text,passw):
    l = min(len(text), len(passw))
    if l == 0: return ""
    return "".join(chr(ord(text[n])^ord(passw[n])) for n in xrange(l) ) + decrypt(text[l:], passw)

text_encrypt = "".join(map(lambda x: chr(x), eval( "[" + open("059-cipher1.txt", "Ur").readline()[:-1] + "]" )))

posible_pass = [chr(i) + chr(j) + chr(k) for i in range(97,123) for j in range(97,123) for k in range(97,123)]

max_num_space = 0
for i in range(len(posible_pass)):
    num_space = decrypt(text_encrypt, posible_pass[i]).count(" ")
    if max_num_space < num_space:
        max_num_space = num_space
        max_num_space_index = i

# max_num_space_index 4423

print sum( ord(n) for n in decrypt(text_encrypt, posible_pass[max_num_space_index]) )