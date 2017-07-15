#!/usr/bin/env python2
# -*- coding: utf-8 -*-

file_tmp = file("042-words.txt","r")
words = eval( "(" + file_tmp.readline() + ")" )
file_tmp.close()

def size_char_word(word):
    size = 0
    for i in word:
        size += ord(i) - 64
    return size

max_word = 0
for i in words:
    word = size_char_word(i)
    if word > max_word:
        max_word = word

sumatoria = [1]
i = 2
while sumatoria[-1] <= max_word:
    sumatoria.append( i + sumatoria[-1] )
    i += 1

total_words_triangle = 0
for i in words:
    try:
        none = sumatoria.index( size_char_word(i) )
        total_words_triangle += 1
    except:
        None

print total_words_triangle


""" by Begoner
wordArray,triangleNums, sumOfWords = eval( '[' + open("042-words.txt").readlines()[ 0 ] + ']' ), [ i * ( i + 1 ) / 2 for i in xrange( 0, 30 ) ], 0
def getValue( word, total = 0 ):
    for i in word: total += ord( i ) - 64
    return total
for i in wordArray:
    if ( getValue( i ) in triangleNums ): sumOfWords += 1
print sumOfWords

=====

wordArray,triangleNums, sumOfWords = eval( '[' + open("042-words.txt").readlines()[ 0 ] + ']' ), [ i * ( i + 1 ) / 2 for i in xrange( 0, 30 ) ], 0
len( [ i for i in [ reduce( lambda x, y: x + ord( y ) - 64, i, 0 ) for i in wordArray ] if i in triangleNums ] )

"""