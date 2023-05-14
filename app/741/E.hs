import qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!))
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import qualified Data.ByteString.Char8 as C


type Arr = A.UArray Int Int


idx :: Int->Int->Int->Int
idx n i j = i*(n+1)+j


greater :: Int -> C.ByteString -> Arr -> Int -> Int -> Bool
greater n str lcp i j | (lcp ! (idx n i j)) == (n-i) = False
greater n str lcp i j | otherwise
  = let lcpScore = lcp ! (idx n i j)
    in
      (C.index str (i + lcpScore)) > (C.index str (j + lcpScore))


score :: Int -> C.ByteString -> Int -> Arr -> Int -> Int -> Int
score n str dpJ lcp i j =
  if greater n str lcp i j then
    dpJ + n - i - (lcp ! (idx n i j))
  else
    0



readInt :: C.ByteString -> Int
readInt bs = case C.readInt bs of
  Nothing -> error "Not an integer"
  Just (x, _) -> x



makeLcp :: Int -> C.ByteString -> Arr
makeLcp n str = runSTUArray $ do
  lcp <- newArray (0, (n+1)*(n+1)-1) 0
  forM_ [n-1,n-2..0] $ \i -> do
    forM_ [n-1,n-2..0] $ \j -> do
      if i==j then do
        writeArray lcp (idx n i j) (n-i)
      else if (C.index str i) /= (C.index str j) then do
        writeArray lcp (idx n i j) 0
      else do
        old <- (readArray lcp (idx n (i+1) (j+1)))
        writeArray lcp (idx n i j) (old + 1)
  return lcp


makeDp :: Int -> C.ByteString -> Arr -> Arr
makeDp n str lcp = runSTUArray $ do
  dp <- newArray (0, n-1) 0
  writeArray dp 0 n
  forM_ [1..n-1] $ \i -> do
    writeArray dp i (n-i)
    forM_ [0..i-1] $ \j -> do
      dpI <- readArray dp i
      dpJ <- readArray dp j
      let val = max dpI (score n str dpJ lcp i j)
      writeArray dp i val
  return dp


maxA :: Int -> Int -> Arr -> Int
maxA x (-1) _ = x
maxA x n dp =
  if (dp ! n) > x then
    maxA (dp ! n) (n-1) dp
  else
    maxA x (n-1) dp


calc :: Int -> C.ByteString -> Int
calc n str =
  let lcp = makeLcp n str
      dp = makeDp n str lcp
  in
    maxA n (n-1) dp


main = do
  line <- C.getLine
  let t = readInt line
  forM_ [1..t] $ \_ -> do
    line <- C.getLine
    let len = readInt line
    str <- C.getLine
    let lcp = makeLcp len str
    let dp = makeDp len str lcp
--    forM_ [0..len] $ \i -> do
--      forM_ [0..len] $ \j -> do
--        putStr $ show (lcp ! (idx len i j))
--        putStr " "
--      putStrLn ""
    -- print lcp
--    print dp
    print $ calc len str
