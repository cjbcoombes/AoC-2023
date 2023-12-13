{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Redundant bracket" #-}
module D3P2 (run) where

import System.IO ()
import Data.Foldable ( Foldable(foldl') )

data Symb = Symb { colAt :: Int, nums :: [Int]} deriving Show

foldi :: (a -> Int -> b -> b) -> b -> [a] -> b
foldi f b l = snd (foldl' (\acc elem -> ((1 + fst acc), (f elem (fst acc) (snd acc)))) (0,b) l)

symbolPos :: String -> [Symb]
symbolPos = foldi (\e i a -> if e == '*' then (Symb i []):a else a) []

-- isSym :: [[Symb]] -> Int -> Int -> Int -> Bool
-- isSym syms row col len = (((col - 1) `elem` syms!!row) || ((col + len) `elem` syms!!row)) ||
--                         (((row > 0) && any (\x -> (x >= (col - 1)) && (x <= (col + len))) (syms!!(row - 1))) ||
--                         ((row < (length syms) - 1) && any (\x -> (x >= (col - 1)) && (x <= (col + len))) (syms!!(row + 1))))

-- sumLine :: [[Symb]] -> Int -> Int -> String -> Int
-- sumLine syms row col line
--   | null line = 0
--   | (head line) `elem` "0123456789" = let n = takeWhile (`elem` "0123456789") line
--                                           len = length n
--                                     in (if (isSym syms row col len) then (read n) else 0) + (sumLine syms row (col + len) (drop len line))
--   | otherwise = sumLine syms row (col + 1) (drop 1 line)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i e l = let sp = splitAt i l
                    in (fst sp) ++ e:(drop 1 (snd sp))

findHelp :: Eq a => Int -> a -> [a] -> Int
findHelp i e l = case l of
                  [] -> -1
                  (x:xs) | x==e -> i
                         | otherwise -> findHelp (i+1) e xs

find :: Eq a => a -> [a] -> Int
find = findHelp 0

replaceAllWhere :: (a -> Bool) -> a -> [a] -> [a]
replaceAllWhere f x = foldr (\e a -> if f e then x:a else e:a) []

pushSymb :: [[Symb]] -> Int -> Int -> Int -> [[Symb]]
pushSymb syms row col n = let r = syms!!row
                              c = head (filter (\x -> col == (colAt x)) r)
                              u = Symb (colAt c) (n:(nums c))
                            in replaceAt row (replaceAllWhere (\x -> col == (colAt x)) u r) syms

symLocs :: [[Symb]] -> Int -> Int -> Int -> [(Int, Int)]
symLocs syms row col len = let sl = map (map colAt) syms
                               s1 = if ((col - 1) `elem` (sl!!row)) then [(row, col-1)] else []
                               s2 = if ((col + len) `elem` (sl!!row)) then (row, col+len):s1 else s1
                               s3 = if (row > 0)
                                    then foldr (\x a -> if (x >= (col - 1)) && (x <= (col + len)) then (row-1, x):a else a) s2 (sl!!(row-1))
                                    else s2
                               s4 = if (row < (length sl) - 1)
                                    then foldr (\x a -> if (x >= (col - 1)) && (x <= (col + len)) then (row+1, x):a else a) s3 (sl!!(row+1))
                                    else s3
                            in s4

-- (((col - 1) `elem` syms!!row) || ((col + len) `elem` syms!!row)) ||
-- (((row > 0) && any (\x -> (x >= (col - 1)) && (x <= (col + len))) (syms!!(row - 1))) ||
-- ((row < (length syms) - 1) && any (\x -> (x >= (col - 1)) && (x <= (col + len))) (syms!!(row + 1))))

putSymb :: [[Symb]] -> Int -> Int -> Int -> Int -> [[Symb]]
putSymb syms row col len n = foldr (\e a -> pushSymb a (fst e) (snd e) n) syms (symLocs syms row col len)
-- (foldi (\e i a -> a ++ (map (\x -> (i, colAt x)) e)) [] syms)


putLine :: [[Symb]] -> Int -> Int -> String -> [[Symb]]
putLine syms row col line
  | null line = syms
  | (head line) `elem` "0123456789" = let n = takeWhile (`elem` "0123456789") line
                                          len = length n
                                          num = (read n) :: Int
                                        in putLine (putSymb syms row col len num) row (col + len) (drop len line)
  | otherwise = putLine syms row (col + 1) (drop 1 line)

doLines :: [String] -> [[Symb]]
doLines strs =
    let syms = map symbolPos strs
    in foldi (\e i a -> putLine a i 0 e) syms strs

sumSyms :: [[Symb]] -> Int
sumSyms syms = sum (map (foldr (\s acc ->
                            case (nums s) of
                                [a, b] -> acc + (a * b)
                                _ -> acc)
                                0)
                                syms)

run :: IO ()
run = do
    contents <- readFile ".\\d3.txt"
    print (doLines (lines contents))
    print (sumSyms (doLines (lines contents)))