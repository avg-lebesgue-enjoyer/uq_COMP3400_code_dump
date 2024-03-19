module Series (Error(..), largestProduct) where
import Data.Char (ord, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | size > length digits   = Left InvalidSpan
    | size < 0               = Left InvalidSpan
largestProduct _ (x:_)
    | x `notElem` ['0'..'9'] = Left $ InvalidDigit x
largestProduct 0 _ = Right 1
largestProduct size thexs@(x:xs) = 
    let d = fromIntegral . digitToInt $ x
        nextProduct = largestProduct size xs
        thisProduct = largestProduct (size - 1) xs >>= (Right . (d *))
    in  among nextProduct thisProduct
    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
    -- m = Either Error
    -- a = Integer
    -- b = Integer
    -- largestProduct (size - 1) xs :: m a
    {-  this :: a -> m a
        this d = case (largestProduct (size - 1) xs) of
                    Left  InvalidSpan       -> Left InvalidSpan
                    Left  (InvalidDigit x)  -> Left (InvalidDigit x)
                    Right product           -> Right (d * product)
    -}
    -- Monad m => m a -> m b -> (a -> b -> m c) -> m c
    -- may as well be
    -- Monad m => m a -> (a -> m b) -> m b      i.e. (>>= f)
    -- composed before                          
    -- Monad m => m b -> (b -> m c) -> m c      i.e. (>>= g)

this :: Either Error Integer -> Integer -> Either Error Integer
this (Left InvalidSpan) _       = Left    InvalidSpan
this (Left (InvalidDigit x)) _  = Left  $ InvalidDigit x
this (Right product) d          = Right $ d * product

-- Try to salvage an error with a genuine value
among   ::  Either Error Integer
        ->  Either Error Integer
        ->  Either Error Integer
among (Left e)  (Left f)  = Left    f
among (Left e)  (Right y) = Right   y
among (Right x) (Left f)  = Right   x
among (Right x) (Right y) = Right $ max x y

what'sTheNumberMate :: Char -> Integer
what'sTheNumberMate '0' = 0
what'sTheNumberMate '1' = 1
what'sTheNumberMate '2' = 2
what'sTheNumberMate '3' = 3
what'sTheNumberMate '4' = 4
what'sTheNumberMate '5' = 5
what'sTheNumberMate '6' = 6
what'sTheNumberMate '7' = 7
what'sTheNumberMate '8' = 8
what'sTheNumberMate '9' = 9
