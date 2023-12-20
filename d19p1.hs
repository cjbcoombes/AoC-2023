{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use first" #-}
module D19P1 (run) where

import Data.Map (Map, empty, insert, (!))



splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s l = let folded = foldr (\e a -> if e == s then ([], fst a:snd a) else (e:fst a, snd a)) ([], []) l
              in fst folded : snd folded

data Part = Part { px :: Int, pm :: Int, pa :: Int, ps :: Int } deriving Show
data Cond = Cond { csrc :: Char, ccomp :: Char, cval :: Int, cgoto :: String } deriving Show
-- cval is x, m, a, s, #
-- ccomp is >, <, *

makeCond :: String -> Cond
makeCond str = if ':' `elem` str
               then let [left, goto] = splitOn ':' str
                        src = head left
                        comp = left !! 1
                        val = read (drop 2 left) :: Int
                    in Cond src comp val goto
               else Cond '#' '*' 0 str

addWf :: Map String [Cond] -> String -> Map String [Cond]
addWf mp wf = let [name, rest] = splitOn '{' wf
                  parts = splitOn ',' (init rest)
                  conds = map makeCond parts
              in insert name conds mp

makePart :: String -> Part
makePart str = let vals = map (read . drop 2) $ splitOn ',' $ tail (init str) :: [Int]
               in Part (vals!!0) (vals!!1) (vals!!2) (vals!!3)

getNext :: Part -> Cond -> Maybe String
getNext part cond = if ccomp cond == '*' ||
                       ((case ccomp cond of { '<' -> (<); '>' -> (>) })
                        ((case csrc cond of { 'x' -> px; 'm' -> pm; 'a' -> pa; 's' -> ps}) part)
                        (cval cond))
                    then Just $ cgoto cond
                    else Nothing

isAccepted :: Map String [Cond] -> [Cond] -> Part -> Bool
isAccepted mp [] part = error "Part failed all"
isAccepted mp (c:cs) part =
    case getNext part c of
        Just str -> (str == "A") || ((str /= "R") && isAccepted mp (mp!str) part)
        Nothing -> isAccepted mp cs part

startAccepted :: Map String [Cond] -> Part -> Bool
startAccepted mp = isAccepted mp (mp!"in")

run :: IO ()
run = do
    contents <- readFile ".\\d19.txt"
    let ls = lines contents
        [ls1, ls2] = splitOn "" ls
        mp = foldl addWf empty ls1
        parts = map makePart ls2
        acc = filter (startAccepted mp) parts
        sum = foldr (\e -> (+ (px e + pm e + pa e + ps e))) 0 acc
    print sum
    print "Done"