module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0 =          Nothing
    | aliquot == n =    Just Perfect
    | aliquot > n =     Just Abundant
    | aliquot < n =     Just Deficient
    where aliquot = aliquotSum n

divisors :: Int -> [Int]
divisors n =
    filter
        (\d -> n `mod` d == 0)
        [1 .. (n - 1)]

aliquotSum :: Int -> Int
aliquotSum = sum . divisors
