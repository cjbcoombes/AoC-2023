module D8P1 (run) where

import System.IO ()

data BST kt vt = Node kt vt (BST kt vt) (BST kt vt) | Leaf deriving Show

bstFind :: Ord kt => BST kt vt -> kt -> Maybe vt
bstFind Leaf x = Nothing
bstFind (Node k v l r) x | x == k = Just v
                         | x < k = bstFind l x
                         | otherwise = bstFind r x

bstInsert :: Ord kt => BST kt vt -> kt -> vt -> BST kt vt
bstInsert Leaf nk nv = Node nk nv Leaf Leaf
bstInsert (Node k v l r) nk nv | nk <= k = Node k v (bstInsert l nk nv) r
                               | otherwise = Node k v l (bstInsert r nk nv)

indexFrom :: Eq a => Int -> a -> [a] -> Int
indexFrom i x [] = error "Element not found"
indexFrom i x (f:r) = if f == x then i else indexFrom (i + 1) x r

indexOf :: Eq a => a -> [a] -> Int
indexOf = indexFrom 0

strHash :: String -> Int
strHash = foldr (\e a -> 26 * a + indexOf e "ABCDEFGHIJKLMNOPQRSTUVWXYZ") 0

lRange :: Int -> Int -> [a] -> [a]
lRange st len = take len . drop st

findZZZ :: BST Int (Int, Int) -> Int -> String -> String -> Int -> Int
findZZZ _ i _ _ 17575 = i                                                   -- ZZZ case
findZZZ bst i str [] cn = findZZZ bst i str str cn                          -- Need to loop
findZZZ bst i str (clr:lrs) cn = case bstFind bst cn of
                                  Just (l, r) -> findZZZ bst (i + 1) str lrs (if clr == 'L' then l else r)
                                  _ -> error ("BST should have this: " ++ show cn ++ "\n" ++ show bst)
                                 

doLine :: String -> BST Int (Int, Int) -> BST Int (Int, Int)
doLine l bst = bstInsert bst (strHash $ lRange 0 3 l) (strHash $ lRange 7 3 l, strHash $ lRange 12 3 l)

doLines :: [String] -> Int
doLines ls = let lrs = head ls
                 bst = foldr doLine Leaf (drop 2 ls)
             in findZZZ bst 0 lrs lrs 0


run :: IO ()
run = do
    contents <- readFile ".\\d8.txt"
    print (doLines (lines contents))

    -- Node 17575 (17575,17575)
    --     (Node 2812 (2812,2812) (Node 0 (703,1406) Leaf Leaf) (Node 1406 (17575,4218) Leaf Leaf))
    --     (Node 4218 (4218,4218) (Node 703 (2109,2812) Leaf Leaf) (Node 2109 (2109,2109) Leaf Leaf))