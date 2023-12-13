module D8P2 (run) where

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
strHash = foldl (\a e -> 26 * a + indexOf e "ABCDEFGHIJKLMNOPQRSTUVWXYZ") 0

strUnhash :: Int -> String
strUnhash n = map ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" !!) [quot n (26^2) `mod` 26, quot n 26 `mod` 26, n `mod` 26]

lRange :: Int -> Int -> [a] -> [a]
lRange st len = take len . drop st

findZ :: BST Int (Int, Int) -> Int -> String -> String -> Int -> Int
findZ _ i _ _ cn | mod cn 26 == 25 = i                                          -- ZZZ case
findZ bst i str [] cn = findZ bst i str str cn                                  -- Need to loop
findZ bst i str (clr:lrs) cn = case bstFind bst cn of
                                  Just (l, r) -> findZ bst (i + 1) str lrs (if clr == 'L' then l else r)
                                  _ -> error ("BST should have this: " ++ show cn ++ "\n" ++ show bst)


doLine :: String -> BST Int (Int, Int) -> BST Int (Int, Int)
doLine l bst = bstInsert bst (strHash $ lRange 0 3 l) (strHash $ lRange 7 3 l, strHash $ lRange 12 3 l)

doLines :: [String] -> Int
doLines ls = let lrs = head ls
                 sts = filter (\x -> mod x 26 == 0) $ map (strHash . lRange 0 3) (drop 2 ls)
                 bst = foldr doLine Leaf (drop 2 ls)
             in foldr lcm 1 $ map (findZ bst 0 lrs lrs) sts


run :: IO ()
run = do
    contents <- readFile ".\\d8.txt"
    print (doLines (lines contents))