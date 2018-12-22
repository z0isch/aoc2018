module Main where

import           Control.Monad.State
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Day17

main :: IO ()
main = do
  part2 >>= print  
  --print $ part2Sol test7
  --part2 . T.unpack <$> TIO.readFile "./input/Day15.txt" >>= print
  --evalStateT part1Sol (S test6 0) >>= print
