module Extra.List where

import qualified Data.List

range :: Int -> Int -> [Int]
range start end = [start .. end - 1]

-- combinations 2 [1,2,3,4] = [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
combinations :: Int -> [Int] -> [[Int]]
combinations size as = filter (\ bs -> length bs == size) (Data.List.subsequences as)
