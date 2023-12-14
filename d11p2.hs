module D11P2 (run) where

import System.IO ()
import Data.Array (Array, array, listArray, assocs, bounds, (!))

type GalArr = Array (Int, Int) Char

arrFromL :: [[a]] -> Array (Int, Int) a
arrFromL ls = let h = length ls
                  w = length (head ls)
              in listArray ((0,0), (h - 1, w - 1)) (concat ls)

getHPref :: GalArr -> [Int]
getHPref arr = let ((_, _), (h, w)) = bounds arr
               in reverse $ snd $ foldl (\(a,l) x -> if all (\y -> arr!(y,x) == '.') [0..h] then (a+1000000, (a+2):l) else (a+1, (a+1):l)) (0,[]) [0..w]

getVPref :: GalArr -> [Int]
getVPref arr = let ((_, _), (h, w)) = bounds arr
               in reverse $ snd $ foldl (\(a,l) y -> if all (\x -> arr!(y,x) == '.') [0..w] then (a+1000000, (a+2):l) else (a+1, (a+1):l)) (0,[]) [0..h]

gdist :: [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
gdist hpref vpref p1 p2 = abs ((hpref !! fst p1) - (hpref !! fst p2)) + abs ((vpref !! snd p1) - (vpref !! snd p2))

doLines :: [String] -> Int -- [((Int, Int), [Int])]
doLines ls = let arr = arrFromL ls
                 gcoords = map ((\(y, x) -> (x, y)) . fst) $ filter ((== '#') . snd) $ assocs arr
                 hpref = getHPref arr
                 vpref = getVPref arr
                 dist = gdist hpref vpref
             in sum $ map sum (foldl (\ds p -> (map (dist p) gcoords):ds) [] gcoords)

run :: IO ()
run = do
    contents <- readFile ".\\d11.txt"
    print $ (`quot` 2) (doLines (lines contents))