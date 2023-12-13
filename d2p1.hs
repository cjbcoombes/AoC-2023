module D2P1 (run) where

import System.IO ()

data MaxData = MaxData {r :: Integer, g :: Integer, b :: Integer}
    deriving Show

maxes :: [String] -> MaxData -> MaxData
maxes strs p = case strs of
    [] -> p
    (n:('r':_):rest) -> maxes (drop 2 strs) (MaxData (max (read n) (r p)) (g p) (b p))
    (n:('g':_):rest) -> maxes (drop 2 strs) (MaxData (r p) (max (read n) (g p)) (b p))
    (n:('b':_):rest) -> maxes (drop 2 strs) (MaxData (r p) (g p) (max (read n) (b p)))
    _ -> maxes (drop 2 strs) p

doLine :: Integer -> String -> Integer
doLine i str = let maxinfo = maxes (drop 2 (words str)) (MaxData 0 0 0)
    in if (((r maxinfo) <= 12) && ((g maxinfo) <= 13) && ((b maxinfo) <= 14))
        then i
        else 0

mapWithIndex :: (Integer -> a -> b) -> Integer -> [a] -> [b]
mapWithIndex f n l = case l of
    [] -> []
    (x:xs) -> (f n x):(mapWithIndex f (n + 1) xs)

run :: IO ()
run = do
    contents <- readFile ".\\d2.txt"
    print (sum (mapWithIndex doLine 1 (lines contents)))
    