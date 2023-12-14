module D10P2 (run) where

import System.IO ()

doLine :: String -> String
doLine l = l

-- Accidentally did all of part 10 in the d10p1.hs file and I'm not too interested in fixing it

run :: IO ()
run = do
    contents <- readFile ".\\sample.txt"
    print (map doLine (lines contents))