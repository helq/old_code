module Main where

main = do 
    let x=[0..9] 
        result = [(a,b,c,d,e,f)|a<-x, b<-x, c<-x, d<-x, e<-x, f<-x, isSum (a,b,c,d,e, f)] 
        k (a,b,c,d,e,f) = a^5 + b^5 + c^5 + d^5 + e^5 + f^5
    print $ (sum $ map k result) - 1

isSum :: (Int, Int, Int, Int, Int, Int) -> Bool
isSum (a,b,c,d,e,f) = a^5 + b^5 + c^5 + d^5 + e^5 + f^5
                      == 100000*a + 10000*b + 1000*c + 100*d + 10*e + f


