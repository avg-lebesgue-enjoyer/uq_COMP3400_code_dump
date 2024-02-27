module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys =  Nothing
    | otherwise =               Just . length . filter pairDifferent $ zip xs ys

pairDifferent :: (Char, Char) -> Bool
pairDifferent = uncurry (/=)
-- pairDifferent (x, y) = (x /= y)
