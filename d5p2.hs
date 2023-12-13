{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module D5P2 (run) where

import System.IO ()

data RangeMap = RangeMap {dest :: Int, src :: Int, rmlen :: Int} deriving Show

data Range = Range {lo :: Int, rlen :: Int} deriving Show

rmap :: RangeMap -> Int -> Int
rmap r x = if (0 <= (x - src r)) && ((x - src r) < rmlen r) then x - src r + dest r else x

rmaplist :: [RangeMap] -> Int -> Int
rmaplist [] x = x
rmaplist (m:ms) x = if (0 <= (x - src m)) && ((x - src m) < rmlen m)
                    then x - src m + dest m
                    else rmaplist ms x

-- Output is (unchanged, changed)
rrmap :: RangeMap -> Range -> ([Range], Maybe Range)
rrmap rm r = let rlo = lo r
                 rhi = rlo + rlen r
                 rmlo = src rm
                 rmhi = src rm + rmlen rm
             in if rlo < rmlo then
                 if rhi <= rmlo then
                  ([r], Nothing)
                 else if rhi <= rmhi then
                  ([Range rlo (rmlo - rlo)], Just $ Range (dest rm) (rhi - rmlo))
                 else
                  ([Range rlo (rmlo - rlo), Range rmhi (rhi - rmhi)], Just $ Range (dest rm) (rmlen rm))
                else if rlo >= rmhi then
                 ([r], Nothing)
                else
                 if rhi <= rmhi then
                  ([], Just $ Range (rlo - rmlo + dest rm) (rlen r))
                 else
                  ([Range rmhi (rhi - rmhi)], Just $ Range (rlo - rmlo + dest rm) (rmhi - rlo))

rrmaplist :: [RangeMap] -> [Range] -> [Range]
rrmaplist [] l = l
rrmaplist (m:ms) l = let mapped = map (rrmap m) l
                         unch = concatMap fst mapped
                         ch = foldr (\e a -> case e of { Nothing -> a; Just x -> x:a }) [] (map snd mapped)
                     in ch ++ rrmaplist ms unch

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s l = let folded = foldr (\e a -> if e == s then ([], fst a:snd a) else (e:fst a, snd a)) ([], []) l
              in fst folded : snd folded

mkRanges :: [Int] -> [Range]
mkRanges [] = []
mkRanges (a:b:r) = Range a b : mkRanges r

doLines :: [String] -> [Range]
doLines ls = let parsed = splitOn "" ls
                 seeds = mkRanges $ map (read :: String -> Int) $ (drop 1 . words . head . head) parsed
                 mapls = map (map (map (read :: String -> Int) . words) . drop 1) (drop 1 parsed)
                 maps = map (map (\[a,b,c] -> RangeMap a b c)) mapls
             in foldl (\a ms -> rrmaplist ms a) seeds maps

run :: IO ()
run = do
    contents <- readFile ".\\d5.txt"
    print $ (minimum . map lo . doLines . lines) contents