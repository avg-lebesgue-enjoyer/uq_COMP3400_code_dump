module Yacht (yacht, Category(..)) where
import Data.List (sort, group, maximumBy)

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

-- Can assume that length dice == 5 and that filter (`in` [1..6]) dice == dice
yacht :: Category -> [Int] -> Int
yacht category dice
    | not (satisfies category dice) =   0
yacht FullHouse dice
    | satisfies Yacht dice =    0
yacht category dice = score category dice


-- Check whether we satisfy the category
satisfies :: Category -> [Int] -> Bool
satisfies Ones _ =      True
satisfies Twos _ =      True
satisfies Threes _ =    True
satisfies Fours _ =     True
satisfies Fives _ =     True
satisfies Sixes _ =     True
satisfies FullHouse dice =      numberOfGroups dice == 2
satisfies FourOfAKind dice =    (numberOfGroups dice >= 1) && not (satisfies FourOfAKind dice)
satisfies LittleStraight dice = sort dice == [1..5]
satisfies BigStraight dice =    sort dice == [2..6]
satisfies Choice dice =         True
satisfies Yacht dice =          numberOfGroups dice == 1

numberOfGroups :: [Int] -> Int
numberOfGroups = length . group . sort



-- Compute score, assuming the category is satisfied, and we're not in
-- the Yacht-FullHouse edge case
score :: Category -> [Int] -> Int
score Ones dice =   scoreNumeric 1 dice
score Twos dice =   scoreNumeric 2 dice
score Threes dice = scoreNumeric 3 dice
score Fours dice =  scoreNumeric 4 dice
score Fives dice =  scoreNumeric 5 dice
score Sixes dice =  scoreNumeric 6 dice
score FullHouse dice =  sum dice
score FourOfAKind dice =
    let groups =        group . sort $ dice
        longestGroup =  maximumBy
                            (\ x y -> compare (length x) (length y))
                            groups
        theFour =       head longestGroup
    in  4 * theFour
score LittleStraight _ =    30
score BigStraight _ =       30
score Choice dice =         sum dice
score Yacht dice =          50

scoreNumeric    :: Int      -- n. dice entered in the "n's" category
                -> [Int]    -- dice
                -> Int      -- score
scoreNumeric n = (n *) . length . filter (== n)
