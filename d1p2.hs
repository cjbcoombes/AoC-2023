module D1P2 (run) where

import System.IO
import Data.List (intercalate)

firstDig :: [Char] -> Integer
firstDig l = case l of
    [] -> error "Error?"
    ('o':'n':'e':_) -> 1
    ('t':'w':'o':_) -> 2
    ('t':'h':'r':'e':'e':_) -> 3
    ('f':'o':'u':'r':_) -> 4
    ('f':'i':'v':'e':_) -> 5
    ('s':'i':'x':_) -> 6
    ('s':'e':'v':'e':'n':_) -> 7
    ('e':'i':'g':'h':'t':_) -> 8
    ('n':'i':'n':'e':_) -> 9
    (c:cs) -> if c `elem` "123456789" then read [c] else firstDig cs

lastDig :: [Char] -> Integer
lastDig l = case l of
    [] -> error "Error?"
    ('e':'n':'o':_) -> 1
    ('o':'w':'t':_) -> 2
    ('e':'e':'r':'h':'t':_) -> 3
    ('r':'u':'o':'f':_) -> 4
    ('e':'v':'i':'f':_) -> 5
    ('x':'i':'s':_) -> 6
    ('n':'e':'v':'e':'s':_) -> 7
    ('t':'h':'g':'i':'e':_) -> 8
    ('e':'n':'i':'n':_) -> 9
    (c:cs) -> if c `elem` "123456789" then read [c] else lastDig cs

score :: [Char] -> Integer
score l = 10 * (firstDig l) + lastDig (reverse l)

run :: IO ()
run = do
    contents <- readFile ".\\d1.txt"
    print (sum (map score (words contents)))