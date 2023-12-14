module D14P1 (run) where

import System.IO ()

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

countOs :: Int -> Int -> String -> Int
countOs _ _ [] = 0
countOs pp rp ('O':ss) = rp + countOs (pp - 1) (rp - 1) ss
countOs pp rp ('#':ss) = countOs (pp - 1) (pp - 1) ss
countOs pp rp (_:ss) = countOs (pp - 1) rp ss

doLine :: String -> Int
doLine l = countOs len len l where len = length l

run :: IO ()
run = do
    contents <- readFile ".\\d14.txt"
    print $ sum (map doLine (transpose $ lines contents))