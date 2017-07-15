#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# by helq

from sys import argv, stderr

def correct(cad, n):
   tmp = cad.split(" --> ")
   return sumTime(tmp[0], n) + " --> " + sumTime(tmp[1], n)

def nEval(strs): # newEval
   if   strs == "08": return 8
   elif strs == "09": return 9
   return eval(strs)

def sumTime(cad, n):
   i = nEval(cad[6:8] + "." + cad[9:]) + nEval(cad[3:5])*60 + nEval(cad[:2])*3600
   i += n
   j = int(i)
   return comp0s(j/3600,2) + ":" + comp0s((j/60)%60,2) + ":" + comp0s(j%60,2) + "," + comp0s(int(i*1000)%1000,3)

comp0s = lambda i,n: str(i).zfill(n)

def messageError():
   stderr.write(argv[0] + " <file-srt-in> -d <delay> [-c <caption>]" +
                          " [-o <file-srt-out>]\n")
   exit(0)

returnArg = lambda a: argv[argv.index(a) + 1]

# Script starts here

if len(argv) < 4 or not "-d" in argv:
   messageError()

n = float( returnArg("-d") )
timeToChange = int( returnArg("-c") ) if "-c" in argv else 0

outFile = False
if "-o" in argv:
   outFile = True
   outName = returnArg("-o")
   fileO = open(outName, "w")

def printFile(a):
   if outFile:
      fileO.write(str(a) + "\n")
   else:
      print a

try:
   a = open(argv[1]).read()
except:
   stderr.write(argv[1] + ": invalid file input\n")
   messageError()

a = a.splitlines()

i, m = 0, 1
while i<len(a):
   printFile(m)
   m+=1
   i+=1
   if eval(a[i-1])>=timeToChange:
      printFile(correct(a[i], n))
   else: printFile(a[i])
   while a[i]!='' and i<len(a)-1:
      i+=1
      printFile(a[i])
   i+=1

if outFile: fileO.close()
