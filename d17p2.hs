{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant return" #-}
module D17P2 (run) where

import Data.Array (Array, listArray, array)
import Data.Array.IO (IOArray, thaw, getBounds, getElems, readArray, writeArray)

data PQueue k v = PQueue { base :: Int, body :: IOArray Int (k, v) }

pqswap :: PQueue k v -> Int -> Int -> IO ()
pqswap pq i j = do t <- readArray (body pq) i
                   readArray (body pq) j >>= writeArray (body pq) i
                   writeArray (body pq) j t

pqdownheap :: (Ord k) => PQueue k v -> Int -> IO ()
pqdownheap pq i =
    do
     let arr = body pq
         li = 2 * (i+1) - 1
         ri = 2 * (i+1) -- + 1 - 1
         lgi1 = i
     lg1 <- readArray arr i
     (lg2, lgi2) <- if li < base pq then
                     readArray arr li >>= (\left -> return $ if fst left < fst lg1 then (left, li) else (lg1, lgi1))
                    else return (lg1, lgi1)
     (lg3, lgi3) <- if ri < base pq then
                     readArray arr ri >>= (\right -> return $ if fst right < fst lg2 then (right, ri) else (lg2, lgi2))
                    else return (lg2, lgi2)
     if lgi3 == i then return ()
     else pqswap pq i lgi3 >> pqdownheap pq lgi3

pqupheap :: (Ord k) => PQueue k v -> Int -> IO ()
pqupheap pq 0 = return ()
pqupheap pq i =
    do
     let arr = body pq
         ti = ((i + (if i `rem` 2 == 1 then 1 else 0)) `quot` 2) - 1
     here <- readArray arr i
     top <- readArray arr ti
     if fst here < fst top then
      pqswap pq i ti >> pqupheap pq ti
     else return ()

pqpop :: (Ord k) => PQueue k v -> IO ((k, v), PQueue k v)
pqpop pq
  | base pq == 0 = error "Pop from empty heap"
  | base pq == 1 = readArray (body pq) 0 >>= (\e -> return (e, pq {base = 0}))
  | otherwise = do e <- readArray (body pq) 0
                   pqswap pq 0 (base pq - 1)
                   let pq' = pq {base = base pq - 1}
                   pqdownheap pq' 0
                   return (e, pq')

pqinsert :: (Ord k) => PQueue k v -> (k, v) -> IO (PQueue k v)
pqinsert pq e = do writeArray (body pq) (base pq) e
                   pqupheap pq (base pq)
                   return pq {base = base pq + 1}

pqprint :: (Show k) => PQueue k v -> IO String
pqprint pq = foldl (\a i -> do { acc <- a; e <- readArray (body pq) i; return $ acc ++ '|':show (fst e)}) (return (show (base pq) ++ "  ")) [0..(base pq - 1)]

data Dir = Horiz | Vert deriving Show
data Node = Node { hMin :: Int, vMin :: Int, hViz :: Bool, vViz :: Bool, cost :: Int} deriving (Eq, Show)
type Arr = IOArray (Int, Int) Node
type NodeQ = PQueue Int (Int, Int, Dir)

arrFromL :: [[Char]] -> Array (Int, Int) Node
arrFromL ls = let h = length ls
                  w = length (head ls)
              in listArray ((0,0), (h - 1, w - 1)) (map (Node 999999999 999999999 False False . read . (:[])) $ concat ls)

printGrid :: Int -> Int -> String -> String
printGrid w 1 str = take w str
printGrid w h str = take w str ++ '\n':printGrid w (h-1) (drop w str)

printArr :: Arr -> IO String
printArr arr = do
                ((_,_), (h, w)) <- getBounds arr
                e <- getElems arr
                return $ printGrid (w+1) (h+1) (map (\(Node _ _ _ _ c) -> head (show c)) e)

printGridCost :: Int -> Int -> [String] -> String
printGridCost w 1 str = concat $ take w str
printGridCost w h str = concat $ take w str ++ ['\n':printGridCost w (h-1) (drop w str)]

printArrCost :: Arr -> IO String
printArrCost arr = do
                    ((_,_), (h, w)) <- getBounds arr
                    e <- getElems arr
                    return $ printGridCost (w+1) (h+1) (map (\(Node m1 m2 _ _ _) -> let s = show (min m1 m2) in if length s == 1 then ' ':' ':s else ' ':s) e)

pushvn :: [Int] -> Int -> Arr -> NodeQ -> Int -> Int -> Int -> IO NodeQ
pushvn is _ _ pq _ _ _ | null is = return pq
pushvn (curr:is) st arr pq x y k =
    do
     (_, (h,_)) <- getBounds arr
     if y + curr >= 0 && y + curr <= h then
      do n <- readArray arr (y + curr, x)
         let (Node _ m _ v c) = n
         pq' <- if (not v) && (k+c < m) && abs curr >= st then
                 do writeArray arr (y+curr, x) (n {vMin=k+c})
                    pqinsert pq (k+c, (x, y+curr, Vert))
                else return pq
         pushvn is st arr pq' x y (k+c)       
     else return pq
         

pushv :: Arr -> NodeQ -> Int -> Int -> Int -> IO NodeQ
pushv arr pq x y k = do pq' <- pushvn [1..10] 4 arr pq x y k
                        pushvn (reverse [(-10)..(-1)]) 4 arr pq' x y k

pushhn :: [Int] -> Int -> Arr -> NodeQ -> Int -> Int -> Int -> IO NodeQ
pushhn is _ _ pq _ _ _ | null is = return pq
pushhn (curr:is) st arr pq x y k =
    do
     (_, (_,w)) <- getBounds arr
     if x + curr >= 0 && x + curr <= w then
      do n <- readArray arr (y, x + curr)
         let (Node m _ v _ c) = n
         pq' <- if (not v) && (k+c < m) && abs curr >= st then
                 do writeArray arr (y, x+curr) (n {hMin=k+c})
                    pqinsert pq (k+c, (x+curr, y, Horiz))
                else return pq
         pushhn is st arr pq' x y (k+c)       
     else return pq
         

pushh :: Arr -> NodeQ -> Int -> Int -> Int -> IO NodeQ
pushh arr pq x y k = do pq' <- pushhn [1..10] 4 arr pq x y k
                        pushhn (reverse [(-10)..(-1)]) 4 arr pq' x y k

iter :: Arr -> NodeQ -> IO NodeQ
iter arr pq = if base pq == 0 then return pq
              else
              do ((k, (x, y, d)), pq') <- pqpop pq
                 --print (k, (x, y, d))
                 n <- readArray arr (y, x)
                 case d of
                    Horiz -> if hViz n then return pq' else
                             do let n' = n {hMin = k, hViz = True}
                                writeArray arr (y, x) n'
                                pushv arr pq' x y k
                    Vert -> if vViz n then return pq' else
                            do let n' = n {vMin = k, vViz = True}
                               writeArray arr (y, x) n'
                               pushh arr pq' x y k

-- Pop queue
-- Get node
-- If it's already visited, leave it

run :: IO ()
run = do
    contents <- readFile ".\\d17.txt"
    arr <- thaw $ arrFromL (lines contents)
    --printArr arr >>= putStrLn
    pq <- thaw (array (0, 20 * length contents) [(0,(0,(0,0,Horiz))), (1,(0,(0,0,Vert)))]) >>= return . PQueue 2 :: IO (PQueue Int (Int, Int, Dir))
    --iter arr pq >>= iter arr >>= pqprint >>= putStrLn
    foldr (\e a -> do pq' <- a
                      pq'' <- iter arr pq'
                      --pqprint pq'' >>= (putStrLn . ((show e ++ "  ") ++))
                      return pq'')
          (return pq) [0..100000]
          >>= (print . base)
    (_, (h,w)) <- getBounds arr
    readArray arr (h,w) >>= (\n -> do { print n; print (min (hMin n) (vMin n)) })
    print "Done"

    -- pq <- thaw (listArray (0, 6) [(4, "hi"), (2, "no"), (7, "weird"), (4, "odd"), (2, "big"), (3, "dup"), (-1, "extra")]) >>= (return . PQueue 6)
    -- pqprint pq >>= putStrLn
    -- pqdownheap pq 0
    -- pqprint pq >>= putStrLn
    -- pqupheap pq 3
    -- pqprint pq >>= putStrLn
    -- (e, pq') <- pqpop pq
    -- print e
    -- pqprint pq' >>= putStrLn
    -- pq'' <- pqinsert pq' (12, "big boi")
    -- pqprint pq'' >>= putStrLn
