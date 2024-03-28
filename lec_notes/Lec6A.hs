module Lec6A where

-- The function >>map<<...
map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs
-- expresses that >> Functor [a] <<

-- The function >>foldr<<...
-- foldr (@) xx (a : b : c : ... : z : [])
-- ==            a @ b @ c @ ... @ z @ xx 
-- with (@) *right* associated,
-- expresses that >>[a]<< is the universal thing satisfying a list diagram.
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _   base []     = base
foldr' (@) base (x:xs) = x   @   foldr' (@) base xs

{- Homework:
    Define takeWhile and dropWhile using hofs
    Define map and filter using foldr
    Define dec2int :: [Int] -> Int with [c,d] |-> + 10c + d using foldl
    Define curry and uncurry using hofs
-}
