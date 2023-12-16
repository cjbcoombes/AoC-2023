module D15P2 (run) where

import System.IO ()
import Data.Char (ord)
import Data.Map (Map, empty, insert, (!), assocs)

type LensMap = Map Int [(String, Int)]

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = uncurry (:) . foldr (\e (c, a) -> if e == d then ([], c:a) else (e:c, a)) ([], [])

hashStr :: String -> Int
hashStr = foldl (\a e -> mod (17 * (a + ord e)) 256) 0

removeWhere :: (a -> Bool) -> [a] -> [a]
removeWhere _ [] = []
removeWhere f (a:as) | f a = as
                     | otherwise = a : removeWhere f as

replaceWhere :: (a -> Bool) -> a -> [a] -> [a]
replaceWhere _ r [] = [r]
replaceWhere f r (a:as) | f a = r : as
                        | otherwise = a : replaceWhere f r as 

doStr :: LensMap -> String -> LensMap
doStr lm str = let txt = takeWhile (`notElem` "-=") str
                   box = hashStr txt
                   lst = lm!box
                   (op:r) = drop (length txt) str
               in if op == '-'
                  then insert box (removeWhere (\(s, _) -> s == txt) lst) lm
                  else insert box (replaceWhere (\(s, _) -> s == txt) (txt, read r) lst) lm


doStrs :: [String] -> Int
doStrs strs = let m = foldr (\i a -> insert i [] a) (empty :: LensMap) [0..255]
                  m' = foldl doStr m strs
              in sum $ map (\(a, bs) -> (a + 1) * (sum $ map (\((_, n), i) -> i * n) (zip bs [1..]))) (assocs m')

run :: IO ()
run = do
    contents <- readFile ".\\d15.txt"
    print $ doStrs (splitOn ',' contents)