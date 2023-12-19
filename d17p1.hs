{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Redundant return" #-}
module D17P1 (run) where

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

pushv :: Arr -> NodeQ -> Int -> Int -> Int -> IO NodeQ
pushv arr pq x y k =
    do
     (_, (h,_)) <- getBounds arr
     --print (k,(x,y,"pushv"))
     l1 <- if y > 0 then
           do
            n1 <- readArray arr (y - 1, x)
            let (Node _ m1 _ v1 c1) = n1
            b1 <- if (not v1) && (k+c1 < m1) then
                   do writeArray arr (y-1, x) (n1 {vMin=k+c1})
                      return [(k+c1, (x, y-1, Vert))]
                   else return []
            if y > 1 then
             do
              n2 <- readArray arr (y - 2, x)
              let (Node _ m2 _ v2 c2) = n2
              b2 <- if (not v2) && (k+c1+c2 < m2) then
                     do writeArray arr (y-2, x) (n2 {vMin=k+c1+c2})
                        return $ (k+c1+c2, (x, y-2, Vert)):b1
                    else return b1
              if y > 2 then
               do
                n3 <- readArray arr (y - 3, x)
                let (Node _ m3 _ v3 c3) = n3
                b3 <- if (not v3) && (k+c1+c2+c3 < m3) then
                       do writeArray arr (y-3, x) (n3 {vMin=k+c1+c2+c3})
                          return $ (k+c1+c2+c3, (x, y-3, Vert)):b2
                      else return b2
                return b3
              else return b2
            else return b1
           else return []
     
     l2 <- if y < h then
           do
            n1 <- readArray arr (y + 1, x)
            let (Node _ m1 _ v1 c1) = n1
            b1 <- if (not v1) && (k+c1 < m1) then
                   do writeArray arr (y+1, x) (n1 {vMin=k+c1})
                      return [(k+c1, (x, y+1, Vert))]
                   else return []
            if y < h-1 then
             do
              n2 <- readArray arr (y + 2, x)
              let (Node _ m2 _ v2 c2) = n2
              b2 <- if (not v2) && (k+c1+c2 < m2) then
                     do writeArray arr (y+2, x) (n2 {vMin=k+c1+c2})
                        return $ (k+c1+c2, (x, y+2, Vert)):b1
                    else return b1
              if y < h-2 then
               do
                n3 <- readArray arr (y + 3, x)
                let (Node _ m3 _ v3 c3) = n3
                b3 <- if (not v3) && (k+c1+c2+c3 < m3) then
                       do writeArray arr (y+3, x) (n3 {vMin=k+c1+c2+c3})
                          return $ (k+c1+c2+c3, (x, y+3, Vert)):b2
                      else return b2
                return b3
              else return b2
            else return b1
           else return []
    
     --print l1
     --print l2
     foldr (\e a -> a >>= (`pqinsert` e)) (return pq) (l1 ++ l2)

pushh :: Arr -> NodeQ -> Int -> Int -> Int -> IO NodeQ
pushh arr pq x y k =
    do
     (_, (_,w)) <- getBounds arr
     --print (k,(x,y,"pushh"))
     l1 <- if x > 0 then
           do
            n1 <- readArray arr (y, x-1)
            let (Node m1 _ v1 _ c1) = n1
            b1 <- if (not v1) && (k+c1 < m1) then
                   do writeArray arr (y, x-1) (n1 {hMin=k+c1})
                      return [(k+c1, (x-1, y, Horiz))]
                   else return []
            if x > 1 then
             do
              n2 <- readArray arr (y, x-2)
              let (Node m2 _ v2 _ c2) = n2
              b2 <- if (not v2) && (k+c1+c2 < m2) then
                     do writeArray arr (y, x-2) (n2 {hMin=k+c1+c2})
                        return $ (k+c1+c2, (x-2, y, Horiz)):b1
                    else return b1
              if x > 2 then
               do
                n3 <- readArray arr (y, x-3)
                let (Node m3 _ v3 _ c3) = n3
                b3 <- if (not v3) && (k+c1+c2+c3 < m3) then
                       do writeArray arr (y, x-3) (n3 {hMin=k+c1+c2+c3})
                          return $ (k+c1+c2+c3, (x-3, y, Horiz)):b2
                      else return b2
                return b3
              else return b2
            else return b1
           else return []
     
     l2 <- if x < w then
           do
            n1 <- readArray arr (y, x+1)
            let (Node m1 _ v1 _ c1) = n1
            b1 <- if (not v1) && (k+c1 < m1) then
                   do writeArray arr (y, x+1) (n1 {hMin=k+c1})
                      return [(k+c1, (x+1, y, Horiz))]
                   else return []
            if x < w - 1 then
             do
              n2 <- readArray arr (y, x+2)
              let (Node m2 _ v2 _ c2) = n2
              b2 <- if (not v2) && (k+c1+c2 < m2) then
                     do writeArray arr (y, x+2) (n2 {hMin=k+c1+c2})
                        return $ (k+c1+c2, (x+2, y, Horiz)):b1
                    else return b1
              if x < w - 2 then
               do
                n3 <- readArray arr (y, x+3)
                let (Node m3 _ v3 _ c3) = n3
                b3 <- if (not v3) && (k+c1+c2+c3 < m3) then
                       do writeArray arr (y, x+3) (n3 {hMin=k+c1+c2+c3})
                          return $ (k+c1+c2+c3, (x+3, y, Horiz)):b2
                      else return b2
                return b3
              else return b2
            else return b1
           else return []
    
     --print l1
     --print l2
     foldr (\e a -> a >>= (`pqinsert` e)) (return pq) (l1 ++ l2)

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
    pq <- thaw (array (0,length contents) [(0,(0,(0,0,Horiz))), (1,(0,(0,0,Vert)))]) >>= return . PQueue 2 :: IO (PQueue Int (Int, Int, Dir))
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
