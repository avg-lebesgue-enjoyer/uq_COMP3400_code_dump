{-
Bezout's lemma solver, built for MATH2301 tute bc I don't wanna 
solve it by hand

@author Gabriel Field uqgfiel1
@date 2024-02-20
-}

module GroupTools where
import Data.List (sort, nub)



{- SECTION: Group type -}

-- Group typeclass!
class Group g where
    (@) :: g -> g -> g
    inv :: g -> g
    one :: g
-- SUBJECT TO the group axioms

-- Compute the group generated by gs in g.
generate    ::  (Group g, Ord g)
            =>  [g] -- gs
            ->  [g] -- result
generate gs =
    let nextGs =    sort . nub $ gs ++ [g @ h | g <- gs, h <- gs]
        theGs =     sort . nub $ gs
    in
        if nextGs == theGs
        then    theGs
        else    generate nextGs

isSubgroup  ::  (Group g, Ord g)
            =>  [g]     -- gs
            ->  Bool    -- result
isSubgroup gs = gs == generate gs



{- SECTION: Common groups I bothered to implement -}

-- Multiplicative integers mod 45.
newtype Mod45 = Mod45 Int
-- Constructor shortcut
m45 :: Int -> Mod45
m45 = Mod45
-- Canonical equivalence classes
classes :: [Int]
classes = filter (\x -> gcd x 45 == 1) [1 .. 44]
-- Check for legality.
legal :: Mod45 -> Bool
legal (Mod45 x) = (x `mod` 45) `elem` classes

instance Show Mod45 where
    show :: Mod45 -> String
    show (Mod45 x) = show (x :: Int)

instance Eq Mod45 where
    (==) :: Mod45 -> Mod45 -> Bool
    (Mod45 x) == (Mod45 y) = x `mod` 45 == y `mod` 45

instance Ord Mod45 where
    (<=) :: Mod45 -> Mod45 -> Bool
    (Mod45 x) <= (Mod45 y) = x <= y

instance Group Mod45 where
    (@) :: Mod45 -> Mod45 -> Mod45
    (Mod45 x) @ (Mod45 y) = Mod45 $ x * y `mod` 45
    inv :: Mod45 -> Mod45 -- NOTE: This function is partial! Only try to invert things that are invertible...
    inv x = head . filter (\y -> y @ x == one) . map Mod45 $ [1..44]
    one :: Mod45
    one = Mod45 1


