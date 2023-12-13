module D1P1 (run) where

import System.IO

firstDig :: [Char] -> Char
firstDig l = case l of
    [] -> error "Error?"
    (c:cs) -> if c `elem` "123456789" then c else firstDig cs

score :: [Char] -> Integer
score l = 10 * read [firstDig l] + read [firstDig $ reverse l]

run :: IO ()
run = do
    contents <- readFile ".\\d1.txt"
    print $ sum $ map score (words contents)