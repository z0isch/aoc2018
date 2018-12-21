module Main where

import           Day15
import Control.Monad.State

main :: IO ()
main = part1 >>= print
  --print $ day14b [8,4,6,0,2,1]
