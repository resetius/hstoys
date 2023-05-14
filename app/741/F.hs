import Control.Monad
import Data.List (sortBy)
import qualified Data.IntMap as IM
import Data.Maybe (fromJust)
import Data.List (all,intercalate)
import System.IO
import Data.Int
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.ByteString.Char8 as C


type I = Int64
type Triplet = (Int,Int,I) -- (i,j,c) = lcm(a_i,b_j)=c
type ITriplet = IM.IntMap [Triplet]
type IMap = IM.IntMap I


readInt :: C.ByteString -> Int
readInt bs = case C.readInt bs of
  Nothing -> error "Not an integer"
  Just (x, _) -> x


readInt64 :: C.ByteString -> Int64
readInt64 bs = case C.readInteger bs of
  Nothing -> error "Not an integer"
  Just (x, _) -> fromIntegral x


sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $ do               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve

primesToUA :: Int -> [Int]
primesToUA top = 2 : [i*2+1 | (i,True) <- assocs $ sieveUA top]


ask (i,j) = do
  hPutStrLn stdout ("? " ++ (show i) ++ " " ++ (show j))
  line <- C.hGetLine stdin
  let lcm = readInt64 line
  return (i,j,lcm)


solveQuadric :: I -> I
solveQuadric lcm =
  let d = sqrt $ fromIntegral (4 * lcm + 1) in
    (round (d+1)) `div` 2


addTriplet' :: ITriplet -> Int -> Triplet -> ITriplet
addTriplet' mp i tr =
  case IM.lookup i mp of
    Just lst -> IM.insert i (tr:lst) mp
    _ -> IM.insert i [tr] mp


addTriplet :: ITriplet -> Triplet -> ITriplet
addTriplet mp tr@(i,j,lcm) =
  let mp' = addTriplet' mp i tr in
    addTriplet' mp' j tr



checkDiv :: I -> Triplet -> Bool
checkDiv q (_,_,lcm) = (lcm `mod` q) == 0


solve100 :: Int -> [Triplet] -> ITriplet -> IMap -> IMap
solve100 _ [] _ lst = lst
solve100 n _ _ lst | IM.size lst == n = lst
solve100 n ((i,j,lcm):xs) mp lst =
  if not ((IM.member i mp) && (IM.member j mp)) then
    solve100 n xs mp lst
  else
    let q = solveQuadric lcm in
      if all (checkDiv q) $ fromJust $ IM.lookup i mp then
        solve100 n xs (IM.delete i mp) $ IM.insert j (q-1) $ IM.insert i q lst
      else
        solve100 n xs (IM.delete j mp) $ IM.insert i (q-1) $ IM.insert j q lst


calc100 n = do
  -- hPutStrLn stderr $ (" > " ++ (show $ solveQuadric 6))
  triplets <- mapM ask [(i,j) | i <- [1..n], j <- [i+1..n]]
  -- hPutStrLn stderr $ show triplets
  let sortedTriplets = sortBy (\(_,_,a) (_,_,b) -> compare b a) triplets
  -- hPutStrLn stderr $ show sortedTriplets
  let tree = foldl addTriplet IM.empty sortedTriplets
  -- hPutStrLn stderr $ show tree
  let res = solve100 n sortedTriplets tree IM.empty
  -- hPutStrLn stderr $ show res
  return $ IM.elems res


maxPrime :: [Int] -> I -> Int
maxPrime (x:xs) a | (a `mod` (fromIntegral x)) == 0 = x
maxPrime (x:xs) a | otherwise = maxPrime xs a
maxPrime _ _ = 1


maxPrimeAndTriplet :: [Int] -> [Triplet] -> (Int,Triplet)
maxPrimeAndTriplet primes tr =
  foldl (\r@(p,_) t@(_,_,a) ->
           let p1 = maxPrime primes a in
             if p1 <= p then r else (p1,t)
           ) (0,(0,0,0)) tr


convRes :: Int -> Int -> Triplet -> I
convRes p j1 (i,_,a) | j1 == i = fromIntegral p
convRes p _ (_,_,a) = a `div` (fromIntegral p)


calc10000 n primes = do
  triplets <- mapM ask [(i,i+1) | i <- [1,3..n-1]]
  let (p,(i,j,lcm)) = maxPrimeAndTriplet primes triplets
  let i1 = if (i > 2) && ((i-1) /= j) then
        i-1
        else if (i > 2) then
        i-2
        else if (i < (n-1)) && ((i+1) /= j) then
        i+1
        else
        i+2
  (_,_,nod) <- ask (i,i1)
  --hPutStrLn stderr $ show (p,nod)
  let j1 = if ((nod `mod` (fromIntegral p)) == 0) then i else j
  res1 <- mapM ask [(i,j1) | i <- [1..j1-1]]
  res2 <- mapM ask [(i,j1) | i <- [j1+1..n]]
  --hPutStrLn stderr $ show res1
  let res1' = map (convRes p j1) res1
  let res2' = map (convRes p j1) res2
  --hPutStrLn stderr $ show (p,t)
  --let p = maxPrime triples primes
  return (res1' ++ ((fromIntegral p):res2'))


main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  hSetBuffering stderr LineBuffering
  let primes = reverse $ primesToUA 200000
  -- hPutStrLn stderr "run"
  line <- hGetLine stdin
  let t = read line :: Int
  -- hPutStrLn stderr ("tests " ++ line)
  forM_ [1..t] $ \_ -> do
    line <- C.hGetLine stdin
    let n = readInt line
    res <- if n <= 100 then
      calc100 n
    else do
      calc10000 n primes
    hPutStr stdout "! "
    -- hPutStrLn stderr $ show res
    hPutStrLn stdout $ intercalate " " $ map show res
