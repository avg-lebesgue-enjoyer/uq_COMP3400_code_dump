import Data.Map

module GraphParser where

data Direction = North | East | West | South
type Array a = [[a]]
-- Directly indexing into an >> Array a << is going to be *very* slow.
-- Let's create a hash map >> (xCoord, yCoord) -> Maybe a <<.

