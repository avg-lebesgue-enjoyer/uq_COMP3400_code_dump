module Lec6B where

-- Type constructors are static  and can't be interacted with at runtime
-- Data constructors are dynamic and can   be interacted with at runtime

-- Type synonym
type Position = (Int, Int)
foo :: [Position] -> Position
foo = foldr (\(a,b) (x,y) -> (a + x, b + y)) (0,0)

-- newtype can be done with data instead.
-- newtype gives efficiency gains at compilation time.
-- newtype has exactly one constructor with exactly one field.
newtype Name = Name String -- fake strings
-- This lets you typecheck at compile-time
bar :: Name -> Int
bar (Name xs) = length xs
seven :: Int
seven = bar $ Name "Big Man"
-- This doesn't typecheck!
-- seven' = bar "Big Man"
