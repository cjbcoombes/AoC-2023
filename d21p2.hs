module D21P2 (run) where

import Debug.Trace
import Data.Array (Array, listArray, assocs, inRange, elems)
import Data.Maybe (isJust)
import Data.Sequence (Seq, empty, fromList, index, (><), singleton)
import qualified Data.Sequence as S (drop)
import Data.Array.ST (STArray, thaw, freeze, readArray, writeArray, getBounds)
import Control.Monad.ST (ST, runST)

data Plot = Start | Garden | Rock deriving (Eq, Show)
data Node = Node Plot (Maybe Int) deriving Show  -- plot, val
data Step = Step Int Int Int deriving Show  -- x, y, val

arrFromL :: [[Char]] -> Array (Int, Int) Node
arrFromL ls = let h = length ls
                  w = length (head ls)
              in listArray ((0,0), (h - 1, w - 1))
                           (map (\l -> Node (case l of { '#' -> Rock; '.' -> Garden; 'S' -> Start}) Nothing) (concat ls))

stepBFS :: STArray s (Int, Int) Node -> Step -> ST s (Seq Step)
stepBFS arr (Step x y sval) = do
    bs <- getBounds arr
    if inRange bs (y, x) then do
        (Node plot nval) <- readArray arr (y, x)
        if plot == Rock || isJust nval then return empty
        else do
            writeArray arr (y, x) (Node plot (Just sval))
            return $ fromList [Step (x-1) y (sval+1), Step (x+1) y (sval+1), Step x (y-1) (sval+1), Step x (y+1) (sval+1)]
    else return empty

stepAllBFS :: STArray s (Int, Int) Node -> Seq Step -> ST s ()
stepAllBFS arr steps
    | null steps = return ()
    | otherwise = let step = index steps 0
                  in stepBFS arr step >>= (\newsteps -> stepAllBFS arr (S.drop 1 steps >< newsteps))


doBFS :: Array (Int, Int) Node -> (Int, Int) -> Array (Int, Int) Node
doBFS arr (sx, sy) = runST $ do
    arr <- (thaw arr :: ST s (STArray s (Int, Int) Node))
    let queue = singleton (Step sx sy 0)
    stepAllBFS arr queue
    freeze arr

countWhere :: (Int -> Bool) -> Array (Int, Int) Node -> Integer
countWhere f arr = foldr (\(Node _ i) a -> a + case i of { Nothing -> 0; Just x -> if f x then 1 else 0}) 0 (elems arr)

run :: IO ()
run = do
    contents <- readFile ".\\d21.txt"
    let arr = arrFromL (lines contents)
        c = (65, 65)
        tl = (0, 0)
        tr = (130, 0)
        bl = (0, 130)
        br = (130, 130)
        tm = (65, 0)
        bm = (65, 130)
        lm = (0, 65)
        rm = (130, 65)
        steps = 26501365 :: Integer
        reach = (steps - 65) `quot` 131 -- 202300 is the number of full "maps" you reach in each direction (the last is not fully covered)
        fullreach = reach - 1 -- The number of fully reached "maps" in each direction
        fullcover = 1 + 2 * fullreach * (fullreach + 1) -- The number of fully covered "maps"
        fullcoverodd = let q = fullreach `quot` 2 in 1 + 4 * q * (q + 1) -- The number of fully covered "maps" with even parity (including the starting one)
        fullcovereven = fullcover - fullcoverodd -- The number of fully covered "maps" with odd parity
        partialcoverpercornertypeA = reach
        partialcoverpercornertypeB = reach - 1 -- (all even)
        edgeremainingvalue = steps - fullreach * 131 - 65 -- = 131
        cornerremainingvalueA = edgeremainingvalue - 65 - 1 -- = 65
        cornerremainingvalueB = cornerremainingvalueA + 131 -- = 65
    let [carr, tlarr, trarr, blarr, brarr, tmarr, bmarr, lmarr, rmarr] = map (doBFS arr) [c, tl, tr, bl, br, tm, bm, lm, rm]
        evencount = countWhere even tlarr
        oddcount = countWhere odd tlarr
        [tlcountA, trcountA, blcountA, brcountA] = map (countWhere (\x -> even x && x <= fromInteger cornerremainingvalueA)) [tlarr, trarr, blarr, brarr]
        [tlcountB, trcountB, blcountB, brcountB] = map (countWhere (\x -> odd x && x <= fromInteger cornerremainingvalueB)) [tlarr, trarr, blarr, brarr]
        [tmcount, bmcount, lmcount, rmcount] = map (countWhere (\x -> even x && x <= fromInteger edgeremainingvalue)) [tmarr, bmarr, lmarr, rmarr]
    let count = fullcovereven * evencount
              + fullcoverodd * oddcount
              + partialcoverpercornertypeA * (tlcountA + trcountA + blcountA + brcountA)
              + partialcoverpercornertypeB * (tlcountB + trcountB + blcountB + brcountB)
              + (tmcount + bmcount + lmcount + rmcount)
    print ("evencount", evencount)
    print ("oddcount", oddcount)
    print ("reach", reach)
    print ("fullreach", fullreach)
    print ("fullcover", fullcover)
    print ("fullcovereven", fullcovereven)
    print ("fullcoverodd", fullcoverodd)
    print ("partialcoverpercornertypeA", partialcoverpercornertypeA)
    print ("partialcoverpercornertypeB", partialcoverpercornertypeB)
    print ("cornerremainingvalueA", cornerremainingvalueA)
    print ("cornerAs", [tlcountA, trcountA, blcountA, brcountA])
    print ("cornerremainingvalueB", cornerremainingvalueB)
    print ("cornerBs", [tlcountB, trcountB, blcountB, brcountB])
    print ("edgeremainingvalue", edgeremainingvalue)
    print ("mids", [tmcount, bmcount, lmcount, rmcount])
    print count
    return ()