module Main where

import           Day15
import Control.Monad.State

main :: IO ()
main = do
    (r,hp) <- runStateT part1Sol (S test5 0)
    print (r,hp)
