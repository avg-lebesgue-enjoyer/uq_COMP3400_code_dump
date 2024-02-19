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
main = main1



{- SECTION: Hello World -}
-- Very very basics:
main1 :: IO ()      -- >> IO () << is an IO action type which results in nothing new
main1 = putStrLn "Hello world"