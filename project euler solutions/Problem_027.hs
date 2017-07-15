module Main where

import Primes (isPrime)

-- quadratic
quad :: Integral t => t -> t -> (t -> t)
quad a b = \x -> x^2 + a*x + b

primesConsec :: Integral t => (t -> t) -> Int
primesConsec quad = length $ takeWhile isPrime $ map quad [0..]

main = print $ maxQuad $ map applyQuad [(a,b) | a<-[-999..999], b<-[2..999]]
    where applyQuad = (\(a,b)->(primesConsec (quad a b), a, b))
          -- maxQuad = foldr1 (\xt@(x,_,_) yt@(y,_,_)->if x>y then xt else yt)
          maxQuad (q:qs) = loop q qs
            where loop q' []    = q'
                  loop q' (q:qs) | q'>q      = loop q' qs
                                 | otherwise = loop q  qs
