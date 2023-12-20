module D18P1 (run) where

import Data.List (zipWith5)

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
        dirs = map ((\[d] -> case d of { 'R' -> R; 'L' -> L; 'U' -> U; 'D' -> D }) . head) splits
        lens = map (read . (!! 1)) splits
        clrs = map (!! 2) splits
        dr = last dirs : init dirs
        dl = tail dirs ++ [head dirs]
        mvs = zipWith5 Move dirs dr dl lens clrs
    print (foldl (flip doLine) (0,0) mvs)
