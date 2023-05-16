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

optOne x ([],_) = x
optOne x (_,[]) = x
optOne (x,n1,m1) ([a],y) = 
    let (z,n2,m2) = optMatMult y
        z1 = a*n2*m2+z 
    in
        if z1 < x then
            (z1,a,m2)
        else
            (x,n1,m1)

optOne (x,n1,m1) (y, [a]) = 
    let (z,n2,m2) = optMatMult $ reverse y
        z1 = n2*m2*a+z
    in
        if z1 < x then
            (z1,n2,a)
        else
            (x,n1,m1)

optOne (x,n1,m1) (y1, y2) = 
    let (z1,n2,m2) = optMatMult $ reverse y1
        (z2,n3,m3) = optMatMult y2
        z3 = z1+z2+n2*m2*m3
    in
        if z3 < x then
            (z3, n2, m3)
        else
            (x, n1, m1)

optMatMult :: [Int] -> (Int,Int,Int) -- (cost,n,m)
optMatMult [] = error "invalid argument" -- imposible
optMatMult [a] = error "invalid argument" -- imposible
optMatMult [a,b] = (0,a,b)
optMatMult x = foldl optOne (maxBound :: Int,0,0) $ splits x
