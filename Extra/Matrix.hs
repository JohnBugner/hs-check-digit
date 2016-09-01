module Extra.Matrix where

import qualified Extra.List

type Matrix a = [[a]]

get :: Matrix a -> Int -> Int -> a
get t y x = (t !! y) !! x

transpose :: Matrix a -> Matrix a
transpose =
    let
        transpose' :: Matrix a -> Matrix a -> Matrix a
        transpose' m' m = case m of
            [] -> m'
            _  -> transpose'
                ((Extra.List.filterMap Extra.List.head m) : m')
                (Extra.List.filterMap Extra.List.tail m)
    in
        reverse . drop 1 . transpose' []
