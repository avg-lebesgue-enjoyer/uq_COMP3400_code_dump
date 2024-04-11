{- This is gorgeous.
-- Test.Quickcheck example.
-}

import Test.QuickCheck (quickCheck, Arbitrary)

-- check1IsDotId :: IO ()
-- check1IsDotId = quickCheck $ \x -> x * 1 == 1 * x

smallest :: [Int] -> Int
smallest = foldr min maxBound

-- Some properties quickchecked:
-- quickCheck $ \xs -> all (\x -> smallest xs <= x) xs
-- quickCheck $ \xs ys -> smallest (xs ++ ys) = smallest xs `min` smallest ys

-- How about Data.List.sort?
-- quickCheck $ \xs -> (sort $ sort xs) == sort xs
-- quickCheck $ \x xs -> let ys = sort (x:xs) in head ys <= minimum ys
-- ascending [] = True
-- ascending (x:[]) = True
-- ascending (x:y:zs) = (x <= y) && ascending (y:zs)
-- quickCheck $ \xs -> ascending . sort $ xs

{-
QuickCheck for half:
    half x = x / 2
    -- QuickChecks:
    quickCheck $ \x -> 2 * (half x) == x
    quickCheck $ \x y -> half (x + y) == half x + half y
    quickCheck $ \x y -> half (x * y) == half x * y
    quickCheck $ \x y -> half (x * y) == x * half y

QuickCheck for quot and rem:
    -- QuickChecks:
    quickCheck $ \x y -> y * (quot x y) + rem x y == x
-}