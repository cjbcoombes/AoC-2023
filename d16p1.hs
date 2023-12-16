module D16P1 (run) where

import Data.Array (Array, listArray)
import Data.Array.IO (IOArray, freeze, thaw, readArray, writeArray, getBounds, getElems)
import Data.Set (Set, empty, notMember, insert)

data Dir = DRight | DLeft | DUp | DDown deriving (Eq, Show, Ord)
data Head = Head { headX :: Int, headY :: Int, headDir :: Dir} deriving (Show, Eq, Ord)
data Mirror = Mirror { beamR :: Bool, beamL :: Bool, beamU :: Bool, beamD :: Bool, mirror :: Char } deriving Show
type Arr = IOArray (Int, Int) Mirror

arrFromL :: [[Char]] -> Array (Int, Int) Mirror
arrFromL ls = let h = length ls
                  w = length (head ls)
              in listArray ((0,0), (h - 1, w - 1)) (map (Mirror False False False False) $ concat ls)

newHeads :: Char -> Head -> [Head]
newHeads '.' h = [case headDir h of
                   DRight -> h { headX = 1 + headX h }
                   DLeft -> h { headX = -1 + headX h }
                   DUp -> h { headY = -1 + headY h }
                   DDown -> h { headY = 1 + headY h }]
newHeads '|' h = case headDir h of
                  DUp -> [h { headY = -1 + headY h }]
                  DDown -> [h { headY = 1 + headY h }]
                  _ -> [Head (headX h) (-1 + headY h) DUp, Head (headX h) (1 + headY h) DDown]
newHeads '-' h = case headDir h of
                  DRight -> [h { headX = 1 + headX h }]
                  DLeft -> [h { headX = -1 + headX h }]
                  _ -> [Head (1 + headX h) (headY h) DRight, Head (-1 + headX h) (headY h) DLeft]
newHeads '\\' h = [case headDir h of
                    DRight -> h { headY = 1 + headY h, headDir = DDown }
                    DLeft -> h { headY = -1 + headY h, headDir = DUp }
                    DUp -> h { headX = -1 + headX h, headDir = DLeft}
                    DDown -> h { headX = 1 + headX h, headDir = DRight }]
newHeads '/' h = [case headDir h of
                   DLeft -> h { headY = 1 + headY h, headDir = DDown }
                   DRight -> h { headY = -1 + headY h, headDir = DUp }
                   DDown -> h { headX = -1 + headX h, headDir = DLeft}
                   DUp -> h { headX = 1 + headX h, headDir = DRight }]

propagate :: Arr -> [Head] -> Set Head -> IO ([Head], Set Head)
propagate arr heads headset =
    do
     ((_,_),(ht,wd)) <- getBounds arr
     let fheads = filter (\h -> h `notMember` headset && headX h >= 0 && headY h >= 0 && headX h <= wd && headY h <= ht) heads
     foldr (\head a ->
             do
              (heads', headset') <- a
              let pos = (headY head, headX head)
              m <- readArray arr pos
              case headDir head of
               DLeft -> writeArray arr pos $ m { beamL = True }
               DRight -> writeArray arr pos $ m { beamR = True }
               DDown -> writeArray arr pos $ m { beamD = True }
               DUp -> writeArray arr pos $ m { beamU = True }
              return (heads' ++ newHeads (mirror m) head, insert head headset')
            ) (return ([], headset)) fheads

propagateAll :: Int -> Arr -> [Head] -> Set Head -> IO ([Head], Set Head)
propagateAll 0 _ heads headset = return (heads, headset)
propagateAll _ _ [] headset = return ([], headset)
propagateAll i arr heads headset =
    do
     (heads', headset') <- propagate arr heads headset
     propagateAll (i-1) arr heads' headset'

countWhere :: (a -> Bool) -> [a] -> Int
countWhere f = foldr (\e a -> a + (if f e then 1 else 0)) 0

printGrid :: Int -> Int -> String -> String
printGrid w 1 str = take w str
printGrid w h str = take w str ++ '\n':printGrid w (h-1) (drop w str)

printArr :: Arr -> IO String
printArr arr = do
                ((_,_), (h, w)) <- getBounds arr
                e <- getElems arr
                return $ printGrid (w+1) (h+1) (map (\(Mirror a b c d _) -> if a || b || c || d then '#' else '.') e)

run :: IO ()
run = do
    contents <- readFile ".\\d16.txt"
    arr <- thaw $ arrFromL (lines contents) :: IO Arr
    let heads = [Head 0 0 DRight]
        headset = empty :: Set Head
    propagateAll 1000 arr heads headset
    elems <- getElems arr
    ((_,_), (h, w)) <- getBounds arr
    print $ countWhere (\(Mirror a b c d _) -> a || b || c || d) elems