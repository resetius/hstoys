import Control.Monad

calc l r | l <= (r `div` 2 + 1) =
           (r-1) `div` 2

calc l r | otherwise = r-l


main = do
  line <- getLine
  let t = read line :: Int
  forM_ [1..t] $ \_ -> do
    line <- getLine
    let [a,b] = map read $ words line :: [Int]
    print $ calc a b
