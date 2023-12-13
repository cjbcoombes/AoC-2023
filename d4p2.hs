module D4P2 (run) where

import System.IO ()

data Card = Card { score :: Int, copies :: Int } deriving Show

cadd :: Int -> Card -> Card
cadd i c = Card (score c) (i + copies c)

initCard :: String -> Card
initCard x = let l = words x
                 wins = map (read :: String -> Int) (take 10 (drop 2 l))
                 cards = map (read :: String -> Int) (drop 13 l)
                 len = length (filter (`elem` wins) cards)
             in Card len 1

procFirst :: [Card] -> [Card]
procFirst cards = let f = head cards
                      u = take (score f) (drop 1 cards)
                      r = drop (1 + score f) cards
                  in map (cadd (copies f)) u ++ r

procAll :: [Card] -> [Card]
procAll [] = []
procAll (f:r) = f : procAll (procFirst (f:r))

doLines :: [String] -> Int
doLines ls = let cards = map initCard ls
             in sum (map copies (procAll cards))

run :: IO ()
run = do
    contents <- readFile ".\\d4.txt"
    print (doLines (lines contents))