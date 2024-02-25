{-
Finite field calculator, realised as (Z/p [x]) / ( i(x) ) for irreducible
i(x) \in \mathbb{Z}/p [x].
Calculator *module* for use in GHCi, not an *application*.

@author Gabriel Field uqgfiel1
@date 2024-02-20
-}

module Main where
--import Data.Char (toUpper)
--import Control.Monad (when, forever)
--import System.IO (openFile, IOMode (..), hGetContents, hClose, withFile)
import System.IO (stdout)
import GHC.IO.Handle (hFlush)

{- LAUNCH -}
main :: IO ()
main = putStrLn "hi"



{- SECTION: Types -}
-- Polynomial [a, b, ...] represents "a + bx + ...".
newtype Polynomial = Polynomial [Integer]
instance Num Polynomial where
    -- desired implementations
    (+) :: Polynomial -> Polynomial -> Polynomial
    (+) (Polynomial xs) (Polynomial ys) =
        let maxLen =    max (length xs) (length ys)
            xs' =       [if i < length xs then xs !! i else 0 | i <- [0..maxLen]]
            ys' =       [if i < length ys then ys !! i else 0 | i <- [0..maxLen]]
        in  Polynomial . reverse . dropWhile (== 0) . reverse $
            zipWith (+) xs' ys'
    (*) :: Polynomial -> Polynomial -> Polynomial
    (*) (Polynomial xs) (Polynomial ys) =
        let sumLen =    length xs + length ys
            xs' =       [if i < length xs then xs !! i else 0 | i <- [0..sumLen]]
            ys' =       [if i < length ys then ys !! i else 0 | i <- [0..sumLen]]
        in  Polynomial . reverse . dropWhile (== 0) . reverse $
            [   sum [   (xs' !! i) * (ys' !! (n - i))
                        | i <- [0..n]
                    ]
                |   n <- [0..sumLen]
            ]
    -- bogus implementations
    abs :: Polynomial -> Polynomial
    abs (Polynomial xs) = Polynomial [0]
    signum :: Polynomial -> Polynomial
    signum (Polynomial xs) = Polynomial [0]
    -- reasonable implementations
    fromInteger :: Integer -> Polynomial
    fromInteger x = Polynomial [x]
    negate :: Polynomial -> Polynomial
    negate (Polynomial xs) = Polynomial . map negate $ xs
instance Show Polynomial where
    show :: Polynomial -> String
    show (Polynomial xs)
        | null xs =     "0"
        | otherwise =   init . init . init . foldl1 (++) $
                        [   show (xs !! i) ++ " x^" ++ show i ++ " + "
                            | i <- [0 .. (length xs - 1)]
                        ]

-- Invert a number x mod p
invert :: Int -> Int -> Int
invert x p = head [y | y <- [0..(p-1)], x * y `mod` p == 1]

-- Reduce a polynomial modulo another
-- ASSUMES that >> last is == 1 <<, i.e. leading coefficient is 1.
pmod :: Polynomial -> Polynomial -> Polynomial
(Polynomial xs) `pmod` (Polynomial is)
    | length xs < length is =   Polynomial xs
    | otherwise =               Polynomial xs `pmod'` Polynomial is

-- Recursive step assuming that length xs >= length is
pmod' :: Polynomial -> Polynomial -> Polynomial
pmod' (Polynomial xs) (Polynomial is) =
    let isEnd = negate . Polynomial . init $ is
    in  (   Polynomial (take (length is - 1) xs)
            + Polynomial (drop (length is - 1) xs) * isEnd
        ) `pmod` Polynomial is

-- Reduce coefficients of a polynomial mod p
cmod :: Polynomial -> Integer -> Polynomial
(Polynomial xs) `cmod` p =
    Polynomial $ map (`mod` p) xs

-- Cartesian product of a list of lists.
-- src: https://stackoverflow.com/a/3388723
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct =
    foldr f [[]]
    where f l a = [ x:xs | x <- l, xs <- a ]

-- FiniteField p (Polynomial is) elements
-- represents the data of a finite field with coefficients mod p
-- over irreducible polynomial (Polynomial is), having element list
-- elements.
data FiniteField = FiniteField Integer Polynomial [Polynomial]

-- Get list for finite field
-- ASSUMES p is prime and positive
-- ASSUMES (Polynomial is) is irreducible mod p and has leading coefficient 1
finiteField :: Integer -> Polynomial -> FiniteField
finiteField p (Polynomial is) =
    FiniteField p (Polynomial is) $
    map Polynomial $
    cartesianProduct (replicate (length is - 1) [0 .. p - 1])

-- Prints a finite field nicely
printFiniteField :: FiniteField -> IO()
printFiniteField (FiniteField p irred elements) = do
    putStrLn $ "Finite field with\n\tcoefficients mod " ++ show p ++ "\n\tpolynomials  mod " ++ show irred
    mapM_ print elements

-- Print off multiplication table for a finite field nicely
multiplicationTable :: FiniteField -> IO ()
multiplicationTable (FiniteField p irred elements) = do
    let product a b = cmod (pmod (a * b) irred) p
        --product a b = pmod (a * b) irred -- <!>
        --product a b = a * b -- <!>
        printNicely (a, b) = putStrLn $ "(" ++ show a ++ ") * (" ++ show b ++ ") = " ++ show (product a b)
    putStrLn "Multiplication table of finite field with:"
    putStrLn $ "\tCoefficients mod " ++ show p
    putStrLn $ "\tPolynomials  mod " ++ show irred
    putStrLn "is..."
    mapM_ printNicely
        [   (a, b)
            |   a <- elements,
                b <- elements
        ]
    -- DEBUG::
    -- putStrLn "<!> Debug..."
    -- let testA = Polynomial [0, 1]
    --     testB = testA
    --     testProduct = testA * testB
    --     (Polynomial testPList) = testProduct
    --     (Polynomial is) = irred
    -- print testProduct
    -- print $ testProduct `pmod` irred
    -- print $ Polynomial $ drop 2 testPList
    -- let isEnd = negate . Polynomial . init $ is
    -- print $ Polynomial (drop 2 testPList) * isEnd
    -- print $ length is - 1
    -- ::DEBUG
    -- stop compiler from whinging
    putStrLn "done"
