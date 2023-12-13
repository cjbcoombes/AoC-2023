module D10P2 (run) where

import System.IO ()

doLine :: String -> String
doLine l = l

run :: IO ()
run = do
    contents <- readFile ".\\sample.txt"
    print (map doLine (lines contents))