import Control.Monad
import Data.List
import Data.Maybe (fromJust)

isEvenChar '1' = True
isEvenChar '4' = True
isEvenChar '6' = True
isEvenChar '8' = True
isEvenChar '9' = True
isEvenChar _ = False


calc1 :: String -> Maybe String
calc1 x = do
  letter <- find isEvenChar x
  return $ [letter]


calc2 :: String -> Maybe String
calc2 (a:'2':xs) = Just (a:['2'])
calc2 (a:'5':xs) = Just (a:['5'])
calc2 (a:b:xs) | a == b = Just (a:[b])
calc2 (a:b:c:xs) | a == c = Just (a:[c])
calc2 ('2':'3':'7':xs) = Just ('2':['7'])
calc2 ('2':'7':xs) = Just ('2':['7'])
calc2 ('5':'3':'7':xs) = Just ('5':['7'])
calc2 ('5':'7':xs) = Just ('5':['7'])
calc2 (a:b:c:xs) = calc2 (b:c:xs)
calc2 _ | otherwise = Nothing


(<||>) :: (Monoid a, Eq a) => a -> a -> a
x <||> y
     | x == mempty = y
     | otherwise = x

-- 77, 33
calc3 :: Char -> String -> Maybe String
calc3 ch x =
  let count = foldr (\a b -> if a == ch then (b+1) else b) 0 x in
    if count >= 2 then Just (ch:[ch])
    else Nothing


calc :: String -> Maybe String
calc a = (calc1 a) <||> (calc2 a ) <||> (calc3 '7' a) <||> (calc3 '3' a)


main = do
  line <- getLine
  let t = read line :: Int
  forM_ [1..t] $ \_ -> do
    line <- getLine
    let k = read line :: Int
    line <- getLine
    let ans1 = calc line
    --print line
    let ans = fromJust ans1
    print $ length ans
    putStrLn ans
