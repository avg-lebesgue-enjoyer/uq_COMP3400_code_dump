module LecMonad.Monad where

--instance Monad [] where
    pure'  :: a -> [a]
    pure' x = [x]
    (>>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>>= f = [y | x <- xs, y <- f x]