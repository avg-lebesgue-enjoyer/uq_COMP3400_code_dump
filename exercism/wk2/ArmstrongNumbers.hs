module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong n = (n == (armstronger n))

-- digits' n returns a list l such that
--  n = sum . reverse $ 
--        [  (l !! i) * (10^i) 
--          |  i <- [0 .. (length l) - 1] 
--        ]
digits' :: Integral a => a -> [a]
digits' n
    | 0 <= n && n < 10 =    [n]
    | otherwise =           (n `mod` 10) : (digits' (n `div` 10))

-- digits n returns a list l such that
--  n = sum
--        [ (l !! i) * (10^i)
--          | i <- [0 .. (length l) - 1]
--        ]
digits :: Integral a => a -> [a]
digits = reverse . digits'

-- armstronger n returns the thing to compare n with
armstronger :: Integral a => a -> a
armstronger n =
    let diggies =       digits n
        numDiggies =    length diggies
        powerList =     map (^numDiggies) diggies
    in  sum powerList
