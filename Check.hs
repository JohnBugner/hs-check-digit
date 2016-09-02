module Check where

import qualified Data.List

import qualified Extra.List
import Extra.Matrix (Matrix)
import qualified Extra.Matrix

limit :: Int
limit = 10

-- Makes the check digit part of the same set as the rest of the digits.
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

-- Detects every 'ab' -> 'ba' error.
isTotallyAntiSymmetric :: Matrix Int -> Bool
isTotallyAntiSymmetric t =
    let
        get' :: Int -> Int -> Int
        get' = Extra.Matrix.get t
        isTotallyAntiSymmetric' :: Int -> Bool
        isTotallyAntiSymmetric' a =
            let
                toTuple :: [a] -> (a, a)
                toTuple as = (as !! 0, as !! 1)
                isTotallyAntiSymmetric'' :: (Int, Int) -> Bool
                isTotallyAntiSymmetric'' (y, x) = (a `get'` y `get'` x) /= (a `get'` x `get'` y)
            in
                all isTotallyAntiSymmetric'' (map toTuple (Extra.List.combinations 2 (Extra.List.range 0 limit)))
    in
        all isTotallyAntiSymmetric' (Extra.List.range 0 limit)

-- Detects every '1a' -> 'a0' error, where is between 3 and 9 inclusive.
isGermanicTotallyAntiSymmetric :: Matrix Int -> Bool
isGermanicTotallyAntiSymmetric t =
    let
        get' :: Int -> Int -> Int
        get' = Extra.Matrix.get t
        isGermanicTotallyAntiSymmetric' :: Int -> Bool
        isGermanicTotallyAntiSymmetric' a =
            let
                isGermanicTotallyAntiSymmetric'' :: Int -> Bool
                isGermanicTotallyAntiSymmetric'' i = (a `get'` 1 `get'` i) /= (a `get'` i `get'` 0)
            in
                all isGermanicTotallyAntiSymmetric'' (Extra.List.range 3 10)
    in
        all isGermanicTotallyAntiSymmetric' (Extra.List.range 0 limit)
