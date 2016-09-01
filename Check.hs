module Check where

import qualified Data.List

import qualified Extra.List
import Extra.Matrix (Matrix)
import qualified Extra.Matrix

limit :: Int
limit = 10

-- Makes 
isClosed :: Matrix Int -> Bool
isClosed =
    let
        isClosed' :: [Int] -> Bool
        isClosed' =
            let
                inRange :: Int -> Bool
                inRange i = (i >= 0) && (i < limit)
            in
                all inRange
    in
        all isClosed'

-- Makes leading 0's not affect the check digit.
isNormal :: Matrix Int -> Bool
isNormal t =
    let
        isNormal' :: Int -> Bool
        isNormal' i = Extra.Matrix.get t 0 i == i
    in
        all isNormal' (Extra.List.range 0 limit)

-- Simplifies checking the check digit at the end.
-- If the list's end value (from 'get') is 0, then it's valid.
isDiagonal :: Matrix Int -> Bool
isDiagonal t =
    let
        isDiagonal' :: Int -> Bool
        isDiagonal' i = Extra.Matrix.get t i i == 0
    in
        all isDiagonal' (Extra.List.range 0 limit)

-- Detects every 'a' -> 'b' error.
isLatin :: Matrix Int -> Bool
isLatin t =
    let
        isLatin' :: [Int] -> Bool
        isLatin' is = length (Data.List.group is) == limit
    in
        (all isLatin' t) && (all isLatin' (Data.List.transpose t))
