module D9P1 (run) where

import System.IO ()

fracL :: [Integer] -> Integer -> Integer -> Integer
fracL s j x = uncurry quot $ foldr (\(e, i) a -> if i == j then a else (fst a * (x - i), snd a * (j - i))) (1,1) (zip s [0..]) 

interpL :: [Integer] -> Integer -> Integer
interpL s x = foldr (\(e, i) a -> a + e * fracL s i x) 0 (zip s [0..])

doLine :: [Integer] -> Integer
doLine l = interpL l (toInteger $ length l)

run :: IO ()
run = do
    contents <- readFile ".\\d9.txt"
    print $ sum (map (doLine . map (read :: String -> Integer) . words) (lines contents))