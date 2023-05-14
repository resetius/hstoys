import Control.Monad
import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))

type Arr = A.UArray Int Int
type Dp = Arr


countLeft' :: [Int] -> (Int,Char) -> [Int]
countLeft' lst@(b:xb) (i,'+') | (even i) = ((b+1):lst)
countLeft' lst@(b:xb) (i,'-') | (even i) = ((b-1):lst)
countLeft' lst@(b:xb) (i,'+') | otherwise = ((b-1):lst)
countLeft' lst@(b:xb) (i,'-') | otherwise = ((b+1):lst)


countLeft :: String -> [Int]
countLeft s =
  reverse $ foldl countLeft' [0] (zip [0..] s)


toArray :: [Int] -> Arr
toArray a = A.array (0,(length a)-1) (zip [0..] a)


prepare :: String -> Dp
prepare x = toArray $ countLeft x


query' :: Int -> Int -> Dp -> Int
query' l r _ | l > r = 0
query' l r dp | odd l = (dp ! r) - (dp ! (l-1))
query' l r dp | otherwise = (dp ! (l-1)) - (dp ! r)


query :: Int -> Int -> Dp -> Int
query l r dp =
  let sum = query' l r dp
  in
    if sum == 0 then 0
    else if even (r-l+1) then 2
    else 1


main = do
  line <- getLine
  let t = read line :: Int
  forM_ [1..t] $ \_ -> do
    line <- getLine
    let [n,q] = map read $ words line :: [Int]
    str <- getLine
    --print str
    let dp = prepare str
    --print dp
    forM_ [1..q] $ \_ -> do
      line <- getLine
      let [l,r] = map read $ words line :: [Int]
      --print [l,r]
      let res = query l r dp
      print res
