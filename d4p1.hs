module D4P1 (run) where

import System.IO ()

doLine :: String -> Int
doLine x = let l = words x
               wins = map (read :: String -> Int) (take 10 (drop 2 l))
               cards = map (read :: String -> Int) (drop 13 l)
               len = length (filter (`elem` wins) cards)
            in if len == 0 then 0 else 2^(len - 1)

run :: IO ()
run = do
    contents <- readFile ".\\d4.txt"
    print $ sum (map doLine (lines contents))