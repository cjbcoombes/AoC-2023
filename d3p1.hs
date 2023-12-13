{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Redundant bracket" #-}
module D3P1 (run) where

import System.IO ()
import Data.Foldable ( Foldable(foldl') )

foldi :: (a -> Int -> b -> b) -> b -> [a] -> b
foldi f b l = snd (foldl' (\acc elem -> ((1 + fst acc), (f elem (fst acc) (snd acc)))) (0,b) l)

symbolPos :: String -> [Int]
symbolPos = foldi (\e i a -> if e `notElem` "0123456789." then i:a else a) []

isSym :: [[Int]] -> Int -> Int -> Int -> Bool
isSym syms row col len = (((col - 1) `elem` syms!!row) || ((col + len) `elem` syms!!row)) ||
                        (((row > 0) && any (\x -> (x >= (col - 1)) && (x <= (col + len))) (syms!!(row - 1))) ||
                        ((row < (length syms) - 1) && any (\x -> (x >= (col - 1)) && (x <= (col + len))) (syms!!(row + 1))))

sumLine :: [[Int]] -> Int -> Int -> String -> Int
sumLine syms row col line
  | null line = 0
  | (head line) `elem` "0123456789" = let n = takeWhile (`elem` "0123456789") line
                                          len = length n
                                    in (if (isSym syms row col len) then (read n) else 0) + (sumLine syms row (col + len) (drop len line))
  | otherwise = sumLine syms row (col + 1) (drop 1 line)

sumLines :: [String] -> Int
sumLines strs =
    let syms = map symbolPos strs
    in sum (foldi (\e i a -> (sumLine syms i 0 e):a) [] strs)

run :: IO ()
run = do
    contents <- readFile ".\\d3.txt"
    print (sumLines (lines contents))