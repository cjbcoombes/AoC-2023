module D18P2 (run) where

import Data.List (zipWith5, elemIndex)

hexToDec' :: Int -> String -> Int
hexToDec' a [] = a
hexToDec' a (x:xs) = hexToDec' (16 * a + (case elemIndex x "0123456789abcdef" of { Just n -> n; Nothing -> error "Bad hex"})) xs

hexToDec :: String -> Int
hexToDec = hexToDec' 0

data Dir = R | L | U | D deriving (Show, Eq)
data Move = Move { mdir :: Dir, pdir :: Dir, ndir :: Dir, mlen :: Int, mcolor :: String } deriving Show

doLine :: Move -> (Int, Int) -> (Int, Int)
doLine (Move d p n l _) (y, a) =
    case d of
        U -> (y-l, a)
        D -> (y+l, a)
        R -> (y, a + (l - 1 + (if p == U then 1 else 0) + (if n == D then 1 else 0)) * (y-1))
        L -> (y, a - (l - 1 + (if p == D then 1 else 0) + (if n == U then 1 else 0)) * y)

run :: IO ()
run = do
    contents <- readFile ".\\d18.txt"
    let splits = map words $ lines contents
        clrs = map (!! 2) splits
        dirs = map ((\d -> case d of { '0' -> R; '2' -> L; '3' -> U; '1' -> D }) . (!! 7)) clrs
        lens = map (hexToDec . take 5 . drop 2) clrs
        dr = last dirs : init dirs
        dl = tail dirs ++ [head dirs]
        mvs = zipWith5 Move dirs dr dl lens clrs
    print (foldl (flip doLine) (0,0) mvs)
