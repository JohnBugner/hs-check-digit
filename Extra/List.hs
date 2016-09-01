module Extra.List where

import qualified Data.Maybe

head :: [a] -> Maybe a
head as =
    if length as == 0
    then Nothing
    else Just (Prelude.head as)

tail :: [a] -> Maybe [a]
tail as =
    if length as == 0
    then Nothing
    else Just (Prelude.tail as)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f as = map Data.Maybe.fromJust (filter Data.Maybe.isJust (map f as))

range :: Int -> Int -> [Int]
range start end = [start .. end - 1]
