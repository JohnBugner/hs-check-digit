module Extra.List where

import qualified Data.Maybe

range :: Int -> Int -> [Int]
range start end = [start .. end - 1]
