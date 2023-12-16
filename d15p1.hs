module D15P1 (run) where

import System.IO ()
import Data.Char (ord)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = uncurry (:) . foldr (\e (c, a) -> if e == d then ([], c:a) else (e:c, a)) ([], [])

hashStr :: String -> Int
hashStr = foldl (\a e -> mod (17 * (a + ord e)) 256) 0

run :: IO ()
run = do
    contents <- readFile ".\\d15.txt"
    print $ sum (map hashStr (splitOn ',' contents))