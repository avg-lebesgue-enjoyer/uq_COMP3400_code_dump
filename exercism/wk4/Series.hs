module Series (slices) where
import Data.Char (ord)

slices :: Int -> String -> [[Int]]
slices n xs = slices' n (map (\x -> ord x - ord '0') xs)

slices' :: Int -> [Int] -> [[Int]]
slices' n xs
    | n > length xs =   []
slices' n xs =
    take n xs : slices' n (drop 1 xs)
