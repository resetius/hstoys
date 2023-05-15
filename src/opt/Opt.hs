module Opt (optFunc, optMatMult, scanrr, splits) where

splits = scanrr (\x (y,_) z -> (x:y, z)) ([],[])

scanrr f z [] = [z]
scanrr f z (x:xs) =
    let (y:ys) = scanrr f z xs in
        f x y xs : (y:ys)

optFunc :: Int -> Int
optFunc x = x

optMatMult :: [Int] -> Int -- cost
optMatMult [] = 1
optMatMult [a] = a
optMatMult [a,b] = a*b
optMatMult _ = error "invalid argument"