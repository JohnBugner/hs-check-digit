module Check where

import qualified Data.List

import Extra.Matrix (Matrix)
import qualified Extra.Matrix (get, transpose)

limit :: Int
limit = 10

-- Leading 0's don't affect the check digit.
isNormal :: Matrix Int -> Bool
isNormal t =
    let
        isNormal' :: Int -> Bool
        isNormal' i = Extra.Matrix.get t 0 i == i
    in
        all isNormal' [0 .. (limit - 1)]

-- Detects every 'a' -> 'b' error.
isLatin :: Matrix Int -> Bool
isLatin t =
    let
        isLatin' :: [Int] -> Bool
        isLatin' is = length (Data.List.group is) == limit
    in
        all isLatin' t && all isLatin' (Extra.Matrix.transpose t)
