module BinarySearch (find) where

import Data.Array ( Array, (!) )
import Data.Array.Base (numElements)

find :: Ord a => Array Int a -> a -> Maybe Int
find array = find' 0 (numElements array) array

find'   ::  Ord a
        =>  Int         -- lower: index to search from (inclusive)
        ->  Int         -- upper: index to search to (exclusive)
        ->  Array Int a -- array: array to find in
        ->  a           -- target: target value to find
        ->  Maybe Int   -- result
find' lower upper array target
    | lower == upper  = Nothing
    | atMid == target = Just middle
    | target < atMid  = find' lower        middle array target
    | atMid  < target = find' (middle + 1) upper  array target
    where
        middle = (upper + lower) `div` 2
        atMid =  array ! middle
