module D21P1 (run) where

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

run :: IO ()
run = do
    contents <- readFile ".\\d21.txt"
    let arr = arrFromL (lines contents)
        ((sy, sx), _):_ = filter (\(_, Node p _) -> p == Start) (assocs arr)
        bfs = doBFS arr (sx, sy)
    print $ foldl (\a (Node _ i) -> a + case i of { Nothing -> 0; Just x -> if odd x && x <= 64 then 1 else 0})
                  0
                  (elems bfs)
    print (sx, sy)