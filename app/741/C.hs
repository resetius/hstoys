import Control.Monad
import Data.List
import Data.Maybe



count1 :: String -> Int
count1 = foldr (\a b -> if a == '1' then (b+1) else b) 0


calc :: Int -> String -> (Int,Int,Int,Int)
calc l x =
  let m = (l `div` 2)
      index0 = findIndex (\a -> a == '0') x
  in
    case index0 of
      Just k | k < m -> (k+2,l,k+1,l)
      Just k | otherwise -> (1,k+1,1,k)
      Nothing -> (1,l-1,2,l)



main = do
  line <- getLine
  let t = read line :: Int
  forM_ [1..t] $ \_ -> do
    line <- getLine
    let l = read line :: Int
    line <- getLine
    let (l1,r1,l2,r2) = calc l line
    putStr $ show l1
    putStr " "
    putStr $ show r1
    putStr " "
    putStr $ show l2
    putStr " "
    putStrLn $ show r2
