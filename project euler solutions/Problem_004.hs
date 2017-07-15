module Main where

palin n = nStr == reverse nStr
    where nStr = show n

maxPalin n = maximum [w*y|y<-[x,x-1..x'],w<-[y,y-1..x'],palin (w*y)]
    where x  = 10^n-1
          x' = 10^(n-1)

main = print $ maxPalin 3
