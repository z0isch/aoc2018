module Main where

import           Control.Monad.State
import           Day15

main :: IO ()
main = do
  part1 >>= print
  --evalStateT part1Sol (S test6 0) >>= print
