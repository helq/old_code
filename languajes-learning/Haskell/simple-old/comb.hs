
-- by helq

fact n = product [1..n]
comb n k = product [n-k+1..n] `div` fact k


comb' _ 0 = 1
comb' n k = sum [comb (n-i) (k-1)| i<-[1..n-k+1]]

