module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isAlphaNum, isAlpha)

responseFor :: String -> String
responseFor xs
    | isQuestion xs && isShouting xs =  "Calm down, I know what I'm doing!"
    | isQuestion xs =                   "Sure."
    | isShouting xs =                   "Whoa, chill out!"
    | isSilence xs =                    "Fine. Be that way!"
    | otherwise =                       "Whatever."

trimJustAlpha :: String -> String
trimJustAlpha = filter isAlpha

trimWhitespace :: String -> String
trimWhitespace = filter (not . isSpace)

trimQuestion :: String -> String
trimQuestion xs =
    if isQuestion xs
    then    init xs
    else    xs

isQuestion :: String -> Bool
isQuestion xs =
    not (null . trimWhitespace $ xs) 
    && ((last . trimWhitespace) xs == '?')

isShouting :: String -> Bool
isShouting xs = 
    not (null . trimJustAlpha $ xs)
    && (all isUpper . trimJustAlpha $ xs)

isSilence :: String -> Bool
isSilence = null . trimWhitespace
