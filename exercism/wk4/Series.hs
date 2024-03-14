module Series (slices) where
import Data.Char (ord)

slices :: Int -> String -> [[Int]]
slices n xs = map (map l2n) $ slices' n xs

slices' :: Int -> String -> [String]
slices' 0 xs  = replicate (length xs) []
slices' n xs
    | n > length xs =   []
slices' n list@(x:xs) =
    take n list : slices' n xs

l2n :: Char -> Int
l2n c = ord c - ord '0'
