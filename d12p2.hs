module D12P2 (run) where

import System.IO ()
import Data.Map (Map, member, (!), empty, insert, assocs)
import Data.List (sort)

type PossMap = Map ([Int], String) Int

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = uncurry (:) . foldr (\e (c, a) -> if e == d then ([], c:a) else (e:c, a)) ([], [])

countWays :: PossMap -> [Int] -> String -> (PossMap, Int)
countWays m [] [] = (m, 1)
countWays m [0] [] = (m, 1)
countWays m [-1] [] = (m, 1)
countWays m _ [] = (m, 0)
countWays m [] (s:ss) = if s == '#'
                        then (insert ([], ss) 0 m, 0)
                        else let (m', res) = countWays m [] ss
                             in (insert ([], ss) res m', res)
countWays m (c:cs) (s:ss) = if member (c:cs,s:ss) m then (m, m ! (c:cs,s:ss)) else
                        let (m', res) = case s of
                                            '.' -> if c <= 0
                                                   then countWays m ((-1):cs) ss
                                                   else (m, 0)
                                            '#' -> if c == 0
                                                   then (m, 0)
                                                   else if c == -1
                                                   then countWays m cs (s:ss)
                                                   else countWays m ((c-1):cs) ss
                                            '?' -> if c == 0
                                                   then countWays m ((-1):cs) ss
                                                   else if c == -1
                                                   then let (m1, r1) = countWays m cs ('#':ss) -- Count as #
                                                            (m2, r2) = countWays m1 (c:cs) ss -- Count as .
                                                        in (m2, r1 + r2)
                                                   else countWays m ((c-1):cs) ss
                        in (insert (c:cs, s:ss) res m', res)
-- countWays m cs ss = error $ "Help? " ++ show cs ++ "  " ++ ss

copies :: Int -> [a] -> [[a]]
copies 0 l = []
copies 1 l = [l]
copies n l = l : copies (n-1) l

joinWith :: a -> [[a]]-> [a]
joinWith d [] = []
joinWith d [l] = l
joinWith d (l:ls) = l ++ d : joinWith d ls

doLine :: String -> (PossMap, Int)
doLine l = let [sstr', nstr] = words l
               sstr = joinWith '?' $ copies 5 sstr'
               nums = concat $ copies 5 $ (:) (-1) $ map (read :: String -> Int) $ splitOn ',' nstr
           in countWays empty nums sstr

pretty :: PossMap -> String
pretty m = foldr (\e a -> e ++ '\n':a) "" $ sort $ map (\((ns, s), i) -> s ++ "      " ++ show ns ++ "      -> " ++ show i) (assocs m)

-- foldr (\n aa -> show n ++ (',':aa)) "" ns

run :: IO ()
run = do
    contents <- readFile ".\\d12.txt"
    -- putStr $ pretty $ fst (doLine $ (!! 5) (lines contents))
    print $ sum (map (snd . doLine) (lines contents))