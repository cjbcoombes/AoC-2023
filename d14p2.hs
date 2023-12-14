{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use <&>" #-}
module D14P2 (run) where

import System.IO ()
import Data.Array (Array, listArray, array)
import Data.Array.IO (IOUArray, freeze, thaw, writeArray, readArray, newArray, getBounds, getElems, getAssocs)
import Data.List (foldl')
import Data.Map (Map, empty, insert, size, member, (!))

type RockArr = IOUArray (Int, Int) Char
type Mem = Map (Array (Int, Int) Char) Int

arrFromL :: [[a]] -> Array (Int, Int) a
arrFromL ls = let h = length ls
                  w = length (head ls)
              in listArray ((0,0), (h - 1, w - 1)) (concat ls)

slideCol :: RockArr -> ((Int, Int) -> (Int, Int)) -> Int -> Int -> Int -> IO ()
slideCol arr ctf w h x =
    do
     c <- foldr (\y a -> do
                          c <- a
                          r <- readArray arr (ctf (y,x))
                          if r == 'O' then writeArray arr (ctf (y,x)) '.' >> return (c+1)
                          else if r == '#' then foldr (\i a -> a >> writeArray arr (ctf (y+i, x)) 'O') (return ()) [1..c] >> return 0
                          else return c)
                (return 0 :: IO Int) [0..h]
     foldr (\i a -> a >> writeArray arr (ctf (i-1, x)) 'O') (return ()) [1..c]
     return ()

slide :: RockArr -> Int -> Int -> IO ()
slide arr w h = do
                 foldr (\i a -> a >> slideCol arr (\(y, x) -> (y, x)) w h i) (return ()) [0..w] --Up
                 foldr (\i a -> a >> slideCol arr (\(x, y) -> (y, x)) h w i) (return ()) [0..h] --Left
                 foldr (\i a -> a >> slideCol arr (\(y', x) -> (h-y', x)) w h i) (return ()) [0..w] --Down
                 foldr (\i a -> a >> slideCol arr (\(x', y) -> (y, w-x')) h w i) (return ()) [0..h] --Right

slideUntilDup :: Int -> Int -> Int -> Int -> RockArr -> Mem -> IO (Int, Int, Mem)
slideUntilDup max i w h _ m | i >= max = return (-1, -1, m)
slideUntilDup max i w h arr m = do
                             slide arr w h
                             --putStr "\n\nIdx: "
                             --putStr $ show i
                             --putStr "\n"
                             --printArr arr >>= putStr
                             frz <- freeze arr
                             if member frz m then return (i, m!frz, m)
                             else slideUntilDup max (i+1) w h arr (insert frz i m)

doLines :: [String] -> IO RockArr
doLines ls = do
              arr <- thaw (arrFromL ls) :: IO RockArr
              ((_,_), (h, w)) <- getBounds arr
              m <- freeze arr >>= (return . (\k -> insert k 0 (empty :: Mem)))
              (i, off, m) <- slideUntilDup 200 1 w h arr m
              let len = i - off
              let push = mod (1000000000 + 1 - off) len
              putStr "\n"
              print off
              print len
              print push
              (i, j, _) <- slideUntilDup push 1 w h arr empty
              print (i, j)
              ass <- getAssocs arr
              let score = sum $ map (\((y, _), c) -> if c == 'O' then h-y+1 else 0) ass
              print score
              return arr

printGrid :: Int -> Int -> String -> String
printGrid w 1 str = take w str
printGrid w h str = take w str ++ '\n':printGrid w (h-1) (drop w str)

printArr :: RockArr -> IO String
printArr arr = do
                ((_,_), (h, w)) <- getBounds arr
                e <- getElems arr
                return $ printGrid (w+1) (h+1) e

run :: IO ()
run = do
    contents <- readFile ".\\d14.txt"
    arr <- doLines (lines contents)
    putStr "Done\n"
    --printArr arr >>= putStr