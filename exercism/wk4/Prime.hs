module Prime (nth) where

nth :: Int -> Maybe Integer
nth = Just . head . primesth

primesth :: Int -> [Integer]
primesth 1 = [2]
primesth n = 
    let primesth' = primesth (n - 1)
        lastPrime = head primesth'
        isPrime x = and [x `mod` p /= 0 | p <- primesth']
        nextPrime = head . filter isPrime $ [lastPrime + 1 ..]
    in  nextPrime : primesth'
