module Lib where

import           Data.List

histogram :: (Eq a, Ord a) => [a] -> [(a,Integer)]
histogram = map (\(x:xs) -> (x, genericLength xs)) . group . sort
