{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use first" #-}
module D19P2 (run) where

import Data.Map (Map, empty, insert, (!))



splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s l = let folded = foldr (\e a -> if e == s then ([], fst a:snd a) else (e:fst a, snd a)) ([], []) l
              in fst folded : snd folded

data Part = Part { px :: (Int, Int), pm :: (Int, Int), pa :: (Int, Int), ps :: (Int, Int) } deriving Show
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

getNext :: Cond -> Part -> [(Part, String)]
getNext cond part =
    if ccomp cond == '*' then [(part, cgoto cond)]
    else let prop = (case csrc cond of { 'x' -> px; 'm' -> pm; 'a' -> pa; 's' -> ps}) part
         in case ccomp cond of
                '<' -> if fst prop < cval cond && snd prop >= cval cond
                       then case csrc cond of
                             'x' -> [(part { px = (fst prop, cval cond - 1) }, cgoto cond), (part { px = (cval cond, snd prop) }, ">>")]
                             'm' -> [(part { pm = (fst prop, cval cond - 1) }, cgoto cond), (part { pm = (cval cond, snd prop) }, ">>")]
                             'a' -> [(part { pa = (fst prop, cval cond - 1) }, cgoto cond), (part { pa = (cval cond, snd prop) }, ">>")]
                             's' -> [(part { ps = (fst prop, cval cond - 1) }, cgoto cond), (part { ps = (cval cond, snd prop) }, ">>")]
                       else [(part, cgoto cond)]
                '>' -> if fst prop <= cval cond && snd prop > cval cond
                       then case csrc cond of
                             'x' -> [(part { px = (fst prop, cval cond) }, ">>"), (part { px = (cval cond + 1, snd prop) }, cgoto cond)]
                             'm' -> [(part { pm = (fst prop, cval cond) }, ">>"), (part { pm = (cval cond + 1, snd prop) }, cgoto cond)]
                             'a' -> [(part { pa = (fst prop, cval cond) }, ">>"), (part { pa = (cval cond + 1, snd prop) }, cgoto cond)]
                             's' -> [(part { ps = (fst prop, cval cond) }, ">>"), (part { ps = (cval cond + 1, snd prop) }, cgoto cond)]
                       else [(part, cgoto cond)]

accepted :: Map String [Cond] -> [Cond] -> Part -> [Part]
accepted _ [] _ = error "Part failed all"
accepted mp (c:cs) part = let next = getNext c part
                              doPart (pt, gt) = case gt of
                                                 "A" -> [pt]
                                                 "R" -> []
                                                 ">>" -> accepted mp cs pt
                                                 s -> accepted mp (mp!s) pt
                          in concatMap doPart next

startAccepted :: Map String [Cond] -> [Part]
startAccepted mp = accepted mp (mp!"in") (Part (1,4000) (1,4000) (1,4000) (1,4000))

countAccepted :: Part -> Integer
countAccepted part = product $ map (\f -> let (lo, hi) = f part in toInteger hi - toInteger lo + 1) [px, pa, ps, pm]

run :: IO ()
run = do
    contents <- readFile ".\\d19.txt"
    let ls = lines contents
        [ls1, _] = splitOn "" ls
        mp = foldl addWf empty ls1
        acc = startAccepted mp
    print $ sum $ map countAccepted acc
    print "Done"