import Control.Monad
import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as C


type Arr = A.UArray Int Int
type Dp = Arr


readInt :: C.ByteString -> Int
readInt bs = case C.readInt bs of
  Nothing -> error "Not an integer"
  Just (x, _) -> x


splitWords :: C.ByteString -> [C.ByteString]
splitWords a = C.split ' ' a



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


check :: Int -> Int -> Int -> Dp -> Int
check l r m dp =
  let left = query' l (m-1) dp
      right = query' (m+1) r dp
  in
  if odd (m-l+1) then
    left + right
  else
    left - right


sameSign :: Int -> Int -> Bool
sameSign a b | a > 0 && b > 0 = True
sameSign a b | a < 0 && b < 0 = True
sameSign _ _ | otherwise = False


findPos' :: Int -> Int -> Int -> Int -> Dp -> Int
findPos' _ _ lb rb dp | lb >= rb = 0
findPos' l r lb rb dp =
  let mb = lb + ((rb-lb) `div` 2)
      lq = check l r lb dp
      mq = check l r mb dp
      rq = check l r rb dp
  in
    if lq == 0 then
      lb
    else if mq == 0 then
      mb
    else if rq == 0 then
      rb
    else if sameSign lq mq then
      findPos' l r mb rb dp
    else
      findPos' l r lb mb dp



findPos :: Int -> Int -> Dp -> Int
findPos l r _ | l == r = l
findPos l r dp | otherwise = findPos' l r l r dp


calc :: Int -> Int -> Dp -> (Int, [Int])
calc l r dp =
  let q = query l r dp in
    if q == 0 then (0, [])
    else if q == 1 then (1, [findPos l r dp])
    else (2, [l,findPos (l+1) r dp])


main = do
  line <- getLine
  let t = read line :: Int
  forM_ [1..t] $ \_ -> do
    line <- C.getLine
    let [n,q] = map readInt $ splitWords line
    str <- getLine
    --print str
    let dp = prepare str
    --print dp
    forM_ [1..q] $ \_ -> do
      line <- C.getLine
      let [l,r] = map readInt $ splitWords line
      --print [l,r]
      let (c,lst) = calc l r dp
      print c
      if c > 0 then do
        putStrLn $ intercalate " " $ map show lst
      else return ()
