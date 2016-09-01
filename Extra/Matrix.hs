module Extra.Matrix where

import qualified Extra.List

type Matrix a = [[a]]

get :: Matrix a -> Int -> Int -> a
get t y x = (t !! y) !! x
