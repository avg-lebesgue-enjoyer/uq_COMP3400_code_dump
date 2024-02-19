{-
Bezout's lemma solver, built for MATH2301 tute bc I don't wanna 
solve it by hand

@author Gabriel Field uqgfiel1
@date 2024-02-20
-}

module Main where
import Data.Char (toUpper)
import Control.Monad (when, forever)
import System.IO (openFile, IOMode (..), hGetContents, hClose, withFile)

{- LAUNCH -}
main :: IO ()
main = putStrLn "placeholder"



{- SECTION: IO interaction -}



{- SECTION: Bezout solver -}
type Big = Int
type CoefficientQuotient = Int
type Small = Int
type Remainder = Int
newtype EuclidStep = EuclidStep (Big, CoefficientQuotient, Small, Remainder)

instance Show EuclidStep where
    show :: EuclidStep -> String
    show (EuclidStep (b, c, s, r)) = "\t" ++ show b ++ "\t= " ++ show c ++ "\t* " ++ show s ++ "\t\t+ " ++ show r ++ "\n"

-- euclidStep big small returns (big, coefficient, small, remainder) such that
-- big = coefficient * small + remainder
euclidStep :: Int -> Int -> EuclidStep
euclidStep small big
    | small < big = euclidStep small big
euclidStep big small =
    let (coefficient, remainder) = big `quotRem` small
    in  EuclidStep (big, coefficient, small, remainder)

-- Empty history version of euclidTrace'
euclidTrace :: Int -> Int -> [EuclidStep]
euclidTrace big small =
    euclidTrace' big small []

-- euclidTrace big small returns the trace.
-- [newest, older, olderer, ..., oldest]
-- This is a fold, but I'm not smart enough to write foldl.
euclidTrace' :: Int -> Int -> [EuclidStep] -> [EuclidStep]
euclidTrace' small big _
    | small < big = euclidTrace' big small []
euclidTrace' big 0 history =
    history
euclidTrace' big small history =
    let EuclidStep (big', coefficient, small', remainder) = euclidStep big small
        newHistory = EuclidStep (big', coefficient, small', remainder) : history
    in  euclidTrace' small remainder newHistory

humanReadableEuclidTrace :: Int -> Int -> [EuclidStep]
humanReadableEuclidTrace big = reverse . euclidTrace big
