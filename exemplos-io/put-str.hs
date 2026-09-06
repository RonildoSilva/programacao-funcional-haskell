module Main where

putStr1 [] = return ()
putStr1 (x:xs) = putChar x >> putStr1 xs

main = do putStr1 "Haskell é legal\n"

