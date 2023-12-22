{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module D22P2 (run) where

import Debug.Trace
import Data.Array (Array, listArray)
import Data.List (sort)
import Data.Map (Map)
import Data.Array.ST (STArray, thaw, freeze, readArray, writeArray, getElems, newArray)
import Control.Monad.ST (ST, runST)

-- Top left corner, bottom right corner, lo and hi z values
data Block = Block { btl :: (Int, Int), bbr :: (Int, Int), blo :: Int, bhi :: Int} deriving (Show, Eq)

instance Ord Block where
    b1 <= b2 = blo b1 <= blo b2

-- Max z value and the block that achieves it
data HeightInfo = HeightInfo { hiz :: Int, hib :: Int } deriving Show

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s l = uncurry (:) $ foldr (\e a -> if e == s then ([], fst a:snd a) else (e:fst a, snd a)) ([], []) l

arrFromL :: [[a]] -> Array (Int, Int) a
arrFromL ls = let h = length ls
                  w = length (head ls)
              in listArray ((0,0), (h - 1, w - 1)) (concat ls)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

makeBlock :: String -> Block
makeBlock str = let [xs, ys, zs] = (\[a, b] -> zip a b) $ map (map (read :: String -> Int) . splitOn ',') (splitOn '~' str)
                    mx = uncurry max xs
              in Block (mapTuple (uncurry min) (xs, ys)) (mapTuple (uncurry max) (xs, ys)) (uncurry min zs) (uncurry max zs)

addBlockSupports :: STArray s (Int, Int) HeightInfo -> STArray s Int [Int] -> Int -> Block -> ST s ()
addBlockSupports arr spts i block = do
    let locs = [(x, y) | x <- [(fst . btl $ block)..(fst . bbr $ block)], y <- [(snd . btl $ block)..(snd . bbr $ block)]]
    hinfos <- foldr (\e a -> a >>= (\l -> readArray arr e >>= return . (:l))) (return []) locs
    let zmax = foldr (max . hiz) 0 hinfos
        bmax = foldr (\(HeightInfo z b) a -> if z == zmax && b `notElem` a then b:a else a) [] hinfos
    writeArray spts i bmax
    let newzmax = zmax + 1 + bhi block - blo block
        hinfo = HeightInfo newzmax i
    foldr (\e a -> a >> writeArray arr e hinfo) (return ()) locs
    return ()

findBlockSupports :: [Block] -> (Int, Int) -> [[Int]]
findBlockSupports blocks (mx, my) = runST $ do
    let numblocks = length blocks
    arr <- newArray ((0, 0), (mx, my)) (HeightInfo 0 (-1)) :: ST s (STArray s (Int, Int) HeightInfo)
    spts <- newArray (0, numblocks - 1) [] :: ST s (STArray s Int [Int])
    foldl (\a (e, i) -> a >> addBlockSupports arr spts i e) (return ()) (zip blocks [0..])
    getElems spts -- return

countFallen :: [[Int]] -> Int -> Int
countFallen spts i = length $ foldl (\a (e, i) -> if not (any (checkNotFallen a) e) then i:a else a) [] (zip spts [0..])
                     where checkNotFallen l e = e `notElem` l && e /= i

run :: IO ()
run = do
    contents <- readFile ".\\d22.txt"
    let blocks = sort (map makeBlock (lines contents))
        xmax = foldr (max . fst . bbr) 0 blocks
        ymax = foldr (max . snd . bbr) 0 blocks
        spts = findBlockSupports blocks (xmax, ymax)
    print $ sum $ map (countFallen spts) [0..(length blocks - 1)]