module D20P1 (run) where

import Debug.Trace
import Prelude hiding (lookup)
import Data.Sequence (Seq, fromList, index, (><), singleton)
import qualified Data.Sequence as S (empty, drop)
import Data.Map (Map, insert, lookup, size)
import qualified Data.Map as M (empty)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s l = let folded = foldr (\e a -> if e == s then ([], fst a:snd a) else (e:fst a, snd a)) ([], []) l
              in fst folded : snd folded

data Pulse = Low | High deriving (Eq, Show)
data Cmd = Cmd String String Pulse deriving (Show)
-- Cmd is from, to, pulse
data Module = Module { mtype :: Char, mname :: String, mstate :: Pulse, mouts :: [String], mins :: Map String Pulse} deriving Show
-- Module is type (%, &, b, *), name, and output names

notPulse :: Pulse -> Pulse
notPulse Low = High
notPulse High = Low

parseMod :: String -> Module
parseMod str = let (l:_:r) = splitOn ' ' str
                   tp = head l
                   name = if tp == 'b' then l else tail l
                   outs = splitOn ',' (concat r)
               in Module tp name Low outs M.empty

doCmd :: Cmd -> Map String Module -> (Map String Module, Seq Cmd)
doCmd (Cmd from name pulse) mp =
    case lookup name mp of
        Nothing -> (mp, S.empty)
        Just (Module tp _ st outs ins) ->
            let newins = insert from pulse ins
                (newst, newout) = case tp of
                                      '%' -> if pulse == Low then (notPulse st, Just (notPulse st)) else (st, Nothing)
                                      '&' -> if all (== High) newins then (Low, Just Low) else (High, Just High)
                                      'b' -> (pulse, Just pulse)
            in (insert name (Module tp name newst outs newins) mp, case newout of { Nothing -> S.empty; Just out -> fromList $ map (\n -> Cmd name n out) outs})

doAllCmds :: (Int, Int) -> Seq Cmd -> Map String Module -> ((Int, Int), Map String Module)
doAllCmds (los, his) cmds mp
    | null cmds = ((los, his), mp)
    | otherwise = let firstcmd = index cmds 0
                      (Cmd _ _ pulse) = firstcmd
                      (mp', newcmds) = doCmd firstcmd mp
                  in doAllCmds (los + if pulse == Low then 1 else 0, his + if pulse == High then 1 else 0) (S.drop 1 cmds >< newcmds) mp'

run :: IO ()
run = do
    contents <- readFile ".\\d20.txt"
    let mods = map parseMod (lines contents)
        modmap' = foldr (\e -> insert (mname e) e) M.empty mods
        modmap = foldr (\(Module _ name _ outs _) a ->
                        foldr (\e a' -> case lookup e a' of { Nothing -> a'; Just m -> insert e (m { mins=insert name Low (mins m)}) a'}) a outs) modmap' mods
    --print modmap
    let ((los, his), mp) = foldr (\i (counts, mp) -> doAllCmds counts (singleton $ Cmd "*button*" "broadcaster" Low) mp) ((0,0), modmap) [1..1000]
    print $ size mp
    print (los, his)
    print (los * his)
    --print mp