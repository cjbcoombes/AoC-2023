module D13P1 (run) where

import System.IO ()
import Data.Array

data Grid = Grid { horiz :: Array Int String, vert :: Array Int String} deriving Show

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = uncurry (:) . foldr (\e (c, a) -> if e == d then ([], c:a) else (e:c, a)) ([], [])

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

makeGrid :: [String] -> Grid
makeGrid strs = Grid (listArray (0, length strs - 1) strs)
                     (let trans = transpose strs
                      in listArray (0, length trans - 1) trans)

checkPair :: Array Int String -> Int -> Int -> Bool
checkPair arr lo hi | (let b = bounds arr in lo < fst b || hi > snd b) = True
                    | arr!lo == arr!hi = checkPair arr (lo - 1) (hi + 1)
                    | otherwise = False

countPairs :: Array Int String -> Int
countPairs arr = foldr (\i a -> if checkPair arr (i-1) i then a + i else a) 0 $ drop 1 $ indices arr

checkGrid :: Grid -> Int
checkGrid g = countPairs (vert g) + 100 * countPairs (horiz g)

run :: IO ()
run = do
    contents <- readFile ".\\d13.txt"
    print $ sum $ map (checkGrid . makeGrid) (splitOn "" $ lines contents)