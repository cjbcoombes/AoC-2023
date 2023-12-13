module D7P1 (run) where

import System.IO ()
import Data.List ( sort )

-- [0, 13) for last
-- 13 * [0,13) for next
-- 13^2 * [0,13) for next
-- 13^3 * [0,13) for next
-- 13^4 * [0,13) for next
-- 13^5 * [0,7) for type

data Hand = Hand { cards :: String, score :: Int, bet :: Int } deriving (Show, Eq)
instance Ord Hand where
    (Hand _ s1 _) `compare` (Hand _ s2 _) = s1 `compare` s2

indexFrom :: Eq a => Int -> a -> [a] -> Int
indexFrom i x [] = -1
indexFrom i x (f:r) = if f == x then i else indexFrom (i + 1) x r

indexOf :: Eq a => a -> [a] -> Int
indexOf = indexFrom 0

countFrom :: Eq a => Int -> a -> [a] -> Int
countFrom n x = foldl (\a e -> if e == x then 1 + a else a) n

countOf :: Eq a => a -> [a] -> Int
countOf = countFrom 0

scoreHand :: String -> Int
scoreHand str =
    let valscore = snd (foldr (\e (p, s) -> (13 * p, s + p * indexOf e "23456789TJQKA")) (1, 0) str)
        counts = map (`countOf` str) "23456789TJQKA"
        countcounts = map (`countOf` counts) [1,2,3,4,5]
        countscore
          | countcounts!!4 >= 1 = 6
          | countcounts!!3 >= 1 = 5
          | countcounts!!2 >= 1 = if countcounts!!1 >= 1 then 4 else 3
          | countcounts!!1 >= 2 = 2
          | countcounts!!1 >= 1 = 1
          | otherwise = 0
    in countscore * 13^5 + valscore

doLine :: String -> Hand
doLine x = let [cards, bet] = words x
           in Hand cards (scoreHand cards) (read bet)

doLines :: [String] -> Int
doLines ls = let hs = sort $ map doLine ls
             in foldr (\e a -> a + uncurry (*) e) 0 $ zip (map bet hs) [1..]

run :: IO ()
run = do
    contents <- readFile ".\\d7.txt"
    print (doLines (lines contents))