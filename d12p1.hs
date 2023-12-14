module D12P1 (run) where

import System.IO ()
import Data.List (foldl')
import Data.Bits (shiftR, (.&.))

countOf :: Eq a => a -> [a] -> Int
countOf x = foldl (\a e -> if e == x then 1 + a else a) 0

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = uncurry (:) . foldr (\e (c, a) -> if e == d then ([], c:a) else (e:c, a)) ([], [])

checkOn :: [Int] -> String -> Bool
checkOn [] [] = True
checkOn [0] [] = True
checkOn _ [] = False
checkOn [] ('.':cs) = null (dropWhile (== '.') cs)
checkOn (0:ns) ('.':cs) = checkOn ns (dropWhile (== '.') cs)
checkOn _ ('.':_) = False
checkOn [] ('#':cs) = False
checkOn (0:ns) ('#':cs) = False
checkOn (n:ns) ('#':cs) = checkOn ((n-1):ns) cs

fillQs :: String -> Int -> String
fillQs str x = reverse . fst $ foldl' (\(out, i) e -> if e == '?'
                                                      then ((if (i .&. 1) == 1 then '#' else '.'):out, shiftR i 1)
                                                      else (e:out, i)) ("", x) str

doLine :: String -> Int --[(String, Bool)]
doLine l = let [sstr, nstr] = words l
               nums = map (read :: String -> Int) $ splitOn ',' nstr
               qs = countOf '?' sstr
               poss = map (dropWhile (== '.') . fillQs sstr) [0..(2^qs - 1)]
               count = foldl' (\a e -> if checkOn nums e then a + 1 else a) 0 poss
           in count --zip poss (map (checkOn nums) poss)

run :: IO ()
run = do
    contents <- readFile ".\\d12.txt"
    --print $ checkOn [0,1,1,3] ".#...#....###."
    --print (doLine $ (!! 3) (lines contents))
    print $ sum (map doLine (lines contents))