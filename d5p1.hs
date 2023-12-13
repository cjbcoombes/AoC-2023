{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use first" #-}
module D5P1 (run) where

import System.IO ()

data RangeMap = RangeMap {dest :: Int, src :: Int, len :: Int} deriving Show

rmap :: RangeMap -> Int -> Int
rmap r x = if (0 <= (x - src r)) && ((x - src r) < len r) then x - src r + dest r else x

rmaplist :: [RangeMap] -> Int -> Int
rmaplist rs x = case rs of
                 [] -> x
                 (m:ms) -> if (0 <= (x - src m)) && ((x - src m) < len m) 
                           then x - src m + dest m
                           else rmaplist ms x

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s l = let folded = foldr (\e a -> if e == s then ([], fst a:snd a) else (e:fst a, snd a)) ([], []) l
              in fst folded : snd folded

doLines :: [String] -> [Int]
doLines ls = let parsed = splitOn "" ls
                 seeds = map (read :: String -> Int) $ (drop 1 . words . head . head) parsed
                 mapls = map (map (map (read :: String -> Int) . words) . drop 1) (drop 1 parsed)
                 maps = map (map (\[a,b,c] -> RangeMap a b c)) mapls
                 flatmaps = concat maps
                 doSeed s = foldl (\a e -> rmaplist e a) s maps
             in map doSeed seeds

run :: IO ()
run = do
    contents <- readFile ".\\d5.txt"
    print $ (minimum . doLines . lines) contents