{-# LANGUAGE BinaryLiterals #-}
module D20P2 (run) where

run :: IO ()
run = print $ lcm (lcm 0b111101010011 0b111110100001) (lcm 0b111111111011 0b111100000111)