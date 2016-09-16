module Check where

import Data.List
import Data.Ix

import Extra.List
import Extra.Matrix
import Extra.Number

limit :: Int
limit = 9

-- Makes the check digit part of the same set as the rest of the digits.
isClosed :: Matrix Int -> Bool
isClosed =
    let
        isClosed' :: [Int] -> Bool
        isClosed' =
            let
                inRange :: Int -> Bool
                inRange i = (i >= 0) && (i <= limit)
            in
                all inRange
    in
        all isClosed'

-- Makes leading 0's not affect the check digit.
isNormal :: Matrix Int -> Bool
isNormal t =
    let
        isNormal' :: Int -> Bool
        isNormal' i = get t 0 i == i
    in
        all isNormal' (range (0, limit))

-- Simplifies checking the check digit at the end.
-- If the list's end value (from 'get') is 0, then it's valid.
isDiagonal :: Matrix Int -> Bool
isDiagonal t =
    let
        isDiagonal' :: Int -> Bool
        isDiagonal' i = get t i i == 0
    in
        all isDiagonal' (range (0, limit))

-- Detects every 'a' -> 'b' error.
isLatin :: Matrix Int -> Bool
isLatin t =
    let
        isLatin' :: [Int] -> Bool
        isLatin' is = length (group is) == inc limit
    in
        (all isLatin' t) && (all isLatin' (transpose t))

-- Detects every 'ab' -> 'ba' error.
isTotallyAntiSymmetric :: Matrix Int -> Bool
isTotallyAntiSymmetric t =
    let
        isTotallyAntiSymmetric' :: Int -> Bool
        isTotallyAntiSymmetric' a =
            let
                toTuple :: [a] -> (a, a)
                toTuple as = (as !! 0, as !! 1)
                isTotallyAntiSymmetric'' :: (Int, Int) -> Bool
                isTotallyAntiSymmetric'' (y, x) =
                    let
                        get' :: Int -> Int -> Int
                        get' = get t
                    in
                        (a `get'` y `get'` x) /= (a `get'` x `get'` y)
            in
                all isTotallyAntiSymmetric'' (map toTuple (combinations 2 (range (0, limit))))
    in
        all isTotallyAntiSymmetric' (range (0, limit))

-- Detects every '1a' -> 'a0' error, where is between 3 and 9 inclusive.
isGermanicTotallyAntiSymmetric :: Matrix Int -> Bool
isGermanicTotallyAntiSymmetric t =
    let
        get' :: Int -> Int -> Int
        get' = get t
        isGermanicTotallyAntiSymmetric' :: Int -> Bool
        isGermanicTotallyAntiSymmetric' a =
            let
                isGermanicTotallyAntiSymmetric'' :: Int -> Bool
                isGermanicTotallyAntiSymmetric'' i = (a `get'` 1 `get'` i) /= (a `get'` i `get'` 0)
            in
                all isGermanicTotallyAntiSymmetric'' (range (3, 9))
    in
        all isGermanicTotallyAntiSymmetric' (range (0, limit))
