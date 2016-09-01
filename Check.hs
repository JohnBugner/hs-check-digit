module Check where

import qualified Data.List

import Extra.Matrix (Matrix)
import qualified Extra.Matrix (get, transpose)

limit :: Int
limit = 10

-- Makes leading 0's not affect the check digit.
isNormal :: Matrix Int -> Bool
isNormal t =
    let
        isNormal' :: Int -> Bool
        isNormal' i = Extra.Matrix.get t 0 i == i
    in
        all isNormal' [0 .. (limit - 1)]

-- Simplifies checking the check digit at the end.
-- If the end value (from 'get') is 0, then the list is valid.
isDiagonal :: Matrix Int -> Bool
isDiagonal t =
    let
        isDiagonal' :: Int -> Bool
        isDiagonal' i = Extra.Matrix.get t i i == 0
    in
        all isDiagonal' [0 .. (limit - 1)]

-- Detects every 'a' -> 'b' error.
isLatin :: Matrix Int -> Bool
isLatin t =
    let
        isLatin' :: [Int] -> Bool
        isLatin' is = length (Data.List.group is) == limit
    in
        all isLatin' t && all isLatin' (Extra.Matrix.transpose t)
