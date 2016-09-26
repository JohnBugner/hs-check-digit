module Check where

import Data.List (group, transpose)
import Data.Ix (range)

import Extra.List (combinations)
import Extra.Matrix (Matrix, get)
import Extra.Number (inc)

checkDigit :: Matrix Int -> [Int] -> Int
checkDigit m = foldl (get m) 0

-- Base 10 Limit
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
isNormal m =
    let
        isNormal' :: Int -> Bool
        isNormal' i = get m 0 i == i
    in
        all isNormal' (range (0, limit))

-- Simplifies checking the check digit at the end.
-- If the list's end value (from 'get') is 0, then it's valid.
isDiagonal :: Matrix Int -> Bool
isDiagonal m =
    let
        isDiagonal' :: Int -> Bool
        isDiagonal' i = get m i i == 0
    in
        all isDiagonal' (range (0, limit))

-- Detects every 'a' -> 'b' error.
isLatin :: Matrix Int -> Bool
isLatin m =
    let
        isLatin' :: [Int] -> Bool
        isLatin' is = length (group is) == inc limit
    in
        (all isLatin' m) && (all isLatin' (transpose m))

-- Detects every 'ab' -> 'ba' error.
isWeakTotallyAntiSymmetric :: Matrix Int -> Bool
isWeakTotallyAntiSymmetric m =
    let
        isWeakTotallyAntiSymmetric' :: Int -> Bool
        isWeakTotallyAntiSymmetric' a =
            let
                toTuple :: [a] -> (a, a)
                toTuple as = (as !! 0, as !! 1)
                isWeakTotallyAntiSymmetric'' :: (Int, Int) -> Bool
                isWeakTotallyAntiSymmetric'' (y, x) =
                    let
                        get' :: Int -> Int -> Int
                        get' = get m
                    in
                        (a `get'` y `get'` x) /= (a `get'` x `get'` y)
            in
                all isWeakTotallyAntiSymmetric'' (map toTuple (combinations 2 (range (0, limit))))
    in
        all isWeakTotallyAntiSymmetric' (range (0, limit))

-- Detects every '1a' -> 'a0' error, where is between 3 and 9 inclusive.
isGermanicTotallyAntiSymmetric :: Matrix Int -> Bool
isGermanicTotallyAntiSymmetric m =
    let
        get' :: Int -> Int -> Int
        get' = get m
        isGermanicTotallyAntiSymmetric' :: Int -> Bool
        isGermanicTotallyAntiSymmetric' a =
            let
                isGermanicTotallyAntiSymmetric'' :: Int -> Bool
                isGermanicTotallyAntiSymmetric'' i = (a `get'` 1 `get'` i) /= (a `get'` i `get'` 0)
            in
                all isGermanicTotallyAntiSymmetric'' (range (3, 9))
    in
        all isGermanicTotallyAntiSymmetric' (range (0, limit))
