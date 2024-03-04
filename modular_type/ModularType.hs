{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
module ModularType where
import Data.Maybe (isNothing)
import Data.Char (ord)



{- SECTION: Mod26 -}

newtype Mod2 = Mod2 Int

m2 :: Int -> Mod2
m2 = Mod2

instance Eq Mod2 where
    (==) :: Mod2 -> Mod2 -> Bool
    (==) (Mod2 x) (Mod2 y) = x == y

instance Show Mod2 where
    show :: Mod2 -> String
    show (Mod2 x) = show $ x `mod` 2

instance Num Mod2 where
    (+) :: Mod2 -> Mod2 -> Mod2
    (+) (Mod2 x) (Mod2 y) = Mod2 $ (x + y) `mod` 2
    (*) :: Mod2 -> Mod2 -> Mod2
    (*) (Mod2 x) (Mod2 y) = Mod2 $ (x * y) `mod` 2
    abs :: Mod2 -> Mod2
    abs (Mod2 x) = Mod2 x
    signum :: Mod2 -> Mod2
    signum _ = Mod2 1
    fromInteger :: Integer -> Mod2
    fromInteger = Mod2 . fromInteger
    negate :: Mod2 -> Mod2
    negate (Mod2 x) = Mod2 $ negate x `mod` 26

instance Fractional Mod2 where
    fromRational :: Rational -> Mod2
    fromRational _ = Mod2 1
    recip :: Mod2 -> Mod2
    recip (Mod2 x)
        | x `mod` 2 == 0 =  error $ show x ++ " is not invertible mod 2 :("
        | x `mod` 2 == 1 =  Mod2 1



{- SECTION: Mod26 -}

newtype Mod26 = Mod26 Int

m :: Int -> Mod26
m = Mod26

instance Eq Mod26 where
    (==) :: Mod26 -> Mod26 -> Bool
    (==) (Mod26 x) (Mod26 y) = x == y

instance Show Mod26 where
    show :: Mod26 -> String
    show (Mod26 x) = show $ x `mod` 26

showl :: Mod26 -> String
showl = show . n2l

instance Num Mod26 where
    (+) :: Mod26 -> Mod26 -> Mod26
    (+) (Mod26 x) (Mod26 y) = Mod26 $ (x + y) `mod` 26
    (*) :: Mod26 -> Mod26 -> Mod26
    (*) (Mod26 x) (Mod26 y) = Mod26 $ (x * y) `mod` 26
    abs :: Mod26 -> Mod26
    abs (Mod26 x) = Mod26 x
    signum :: Mod26 -> Mod26
    signum _ = Mod26 1
    fromInteger :: Integer -> Mod26
    fromInteger = Mod26 . fromInteger
    negate :: Mod26 -> Mod26
    negate (Mod26 x) = Mod26 $ negate x `mod` 26

instance Fractional Mod26 where
    fromRational :: Rational -> Mod26
    fromRational _ = Mod26 1
    recip :: Mod26 -> Mod26
    recip x
        | isNothing y =     error $ show x ++ " is not invertible mod 26 :("
        | otherwise =       let (Just y') = y in y'
        where y = maybeRecip x

maybeRecip :: Mod26 -> Maybe Mod26
maybeRecip (Mod26 x) =
    let reciprocals =   [   y
                        |   y <- [1..25],
                                m x * m y == m 1
                        ]
    in  if null reciprocals
        then Nothing
        else Just . m . head $ reciprocals



{- SECTION: Number-letter conversions -}

english :: [Char]
english = ['a'..'z']

n2l :: Mod26 -> Char
n2l (Mod26 x) = english !! (x `mod` 26)

l2n :: Char -> Mod26
l2n c = m $ ord c - ord 'a'
