module D10P1 (run) where

import Prelude hiding (Left, Right)
import System.IO ()
import Data.Array
import Data.List (splitAt, sort)

data Arr2d a = Arr2d { arr :: Array (Int, Int) a, w :: Int, h :: Int } deriving Show

data Dir = Up | Down | Left | Right deriving (Eq, Show, Ord)

nextDir :: Char -> Dir -> Dir
nextDir '-' Left = Left
nextDir '-' Right = Right
nextDir '|' Up = Up
nextDir '|' Down = Down
nextDir 'F' Left = Down
nextDir 'F' Up = Right
nextDir 'J' Right = Up
nextDir 'J' Down = Left
nextDir '7' Right = Down
nextDir '7' Up = Left
nextDir 'L' Left = Up
nextDir 'L' Down = Right
nextDir c d = error ("nextDir: " ++ c:' ':show d)

nextPos :: (Int, Int) -> Dir -> (Int, Int)
nextPos (x,y) Left = (x-1, y)
nextPos (x,y) Right = (x+1, y)
nextPos (x,y) Up = (x, y-1)
nextPos (x,y) Down = (x, y+1)

arrFromL :: [[a]] -> Arr2d a
arrFromL ls = let h = length ls
                  w = length (head ls)
              in Arr2d (listArray ((0,0), (h - 1, w - 1)) (concat ls)) w h

findS :: Arr2d Char -> (Int, Int)
findS a = head $ map fst (filter (\(_, c) -> c == 'S') $ assocs (arr a))

gin :: (Int, Int, Dir, Dir) -> Dir
gin (_,_,d,_) = d

gout :: (Int, Int, Dir, Dir) -> Dir
gout (_,_,_,d) = d

gst :: (Int, Int, Dir, Dir) -> Int
gst (d,_,_,_) = d

gend :: (Int, Int, Dir, Dir) -> Int
gend (_,d,_,_) = d

stepTo :: Arr2d Char -> Char -> [[(Int, Int, Dir, Dir)]] -> (Int, Int) -> Dir -> [[(Int, Int, Dir, Dir)]]
stepTo a tgt i curr dir = let c = arr a ! (snd curr, fst curr)
                          in if tgt == c then i
                             else let ndir = nextDir c dir
                                      npos = nextPos curr ndir
                                      x = fst curr
                                      newi = let (a, b:c) = splitAt (snd curr) i
                                             in (\nb -> a ++ nb:c) (
                                                case b of
                                                    [] -> [(x, x, dir, ndir)]
                                                    (f:r) -> case dir of
                                                                Right -> (gst f, gend f + 1, gin f, ndir):r
                                                                Left -> (gst f - 1, gend f, gin f, ndir):r
                                                                _ -> (x, x, dir, ndir):f:r
                                                )
                                  in stepTo a tgt newi npos ndir

xor :: Bool -> Bool -> Bool
xor = (/=)

scoreRun :: Bool -> [(Int, Int, Dir, Dir)] -> Int
scoreRun _ [] = 0
scoreRun _ [_] = 0
scoreRun leftin (a:b:r) = let midin = leftin `xor` (gin a == gout a)
                          in (if midin then gst b - gend a - 1 else 0) + scoreRun midin (b:r)

doLines :: [String] -> [Int] -- [[(Int, Int, Dir, Dir)]]
doLines ls = let a = arrFromL ls
                 s = (\(y,x) -> (x,y)) $ findS a
                 ps = [([(fst s, fst s, Down, Left) | (y-1) == snd s]) | y <- [1..(h a)]]
             in map (scoreRun False . sort) $ stepTo a 'S' ps (nextPos s Left) Left

run :: IO ()
run = do
    contents <- readFile ".\\d10.txt"
    print $ sum (doLines (lines contents))