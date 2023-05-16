module Opt (optFunc, optMatMult, scanrr, splits, splits1) where

splits1 :: [a] -> [a] -> [([a],[a])] -> [([a],[a])]
splits1 [] x acc = (x,[]) : acc
splits1 (x:xs) y acc = splits1 xs (x:y) ((y,x:xs):acc)

splits x =  splits1 x [] []

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