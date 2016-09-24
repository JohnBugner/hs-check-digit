module Main where

import Check

import Extra.Matrix (Matrix)

main :: IO ()
main = do
    check tableA
    check tableB
    check tableC

check :: Matrix Int -> IO ()
check table = do
    print' "--------------------"
    print' $ concat [show (isClosed table), " : closed"]
    print' $ concat [show (isNormal table), " : normal"]
    print' $ concat [show (isDiagonal table), " : diagonal"]
    print' $ concat [show (isLatin table), " : latin"]
    print' $ concat [show (isTotallyAntiSymmetric table), " : totally anti-symmetric"]
    print' $ concat [show (isGermanicTotallyAntiSymmetric table), " : germanic totally anti-symmetric"]
    print' "--------------------"
    where
        print' :: String -> IO ()
        print' = putStrLn . id

-- Tables were created by Michael Damm.
tableA :: Matrix Int
tableA =
    [ [0,3,1,7,5,9,8,6,4,2]
    , [7,0,9,2,1,5,4,8,6,3]
    , [4,2,0,6,8,7,1,3,5,9]
    , [1,7,5,0,9,8,3,4,2,6]
    , [6,1,2,3,0,4,5,9,7,8]
    , [3,6,7,4,2,0,9,5,8,1]
    , [5,8,6,9,7,2,0,1,3,4]
    , [8,9,4,5,3,6,2,0,1,7]
    , [9,4,3,8,6,1,7,2,0,5]
    , [2,5,8,1,4,3,6,7,9,0]
    ]

tableB :: Matrix Int
tableB =
    [ [0,1,2,3,4,5,6,7,8,9]
    , [3,0,5,9,2,4,8,6,7,1]
    , [8,9,0,7,6,3,2,1,4,5]
    , [2,3,4,0,5,6,1,8,9,7]
    , [7,2,9,1,0,8,4,5,3,6]
    , [1,7,3,8,9,0,5,4,6,2]
    , [4,6,7,5,3,9,0,2,1,8]
    , [6,5,8,4,1,7,9,0,2,3]
    , [5,8,1,6,7,2,3,9,0,4]
    , [9,4,6,2,8,1,7,3,5,0]
    ]

tableC :: Matrix Int
tableC =
    [ [0,1,2,3,4,5,6,7,8,9]
    , [2,3,0,1,8,7,9,6,5,4]
    , [3,2,1,0,7,9,8,5,4,6]
    , [1,0,3,2,9,8,7,4,6,5]
    , [5,9,8,7,6,0,4,2,1,3]
    , [6,8,7,9,5,4,0,1,3,2]
    , [4,7,9,8,0,6,5,3,2,1]
    , [8,5,4,6,3,2,1,9,0,7]
    , [9,4,6,5,2,1,3,8,7,0]
    , [7,6,5,4,1,3,2,0,9,8]
    ]
