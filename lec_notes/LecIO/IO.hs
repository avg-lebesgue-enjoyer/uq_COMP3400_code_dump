{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use hPrint" #-}
module LecIO.IO where

import System.IO hiding () -- ( Handle, IOMode (..), openFile, hClose, hGetLine ) 



{- SECTION: IO -}

main :: IO ()
main = do
    return ()

putStrLn' :: String -> IO ()
putStrLn' []     = putChar '\n'
putStrLn' (c:cs) = do
    putChar c
    putStrLn' cs

triplePrint :: IO ()
triplePrint = do
    char <- getChar
    putStrLn' [char, char, char]

getLine' :: IO String
getLine' = do
    char <- getChar
    case char of
        '\n' -> return ""
        _    -> do
            line <- getLine'
            return $ char : line

print' :: Show a => a -> IO ()
print' = putStrLn' . show

stringLength :: IO ()
stringLength = do
    putStrLn' "Enter a string pls"
    print . length =<< getLine'

readInt :: String -> Integer
readInt xs = read xs :: Integer

(~) :: (a -> b) -> (b -> c) -> (a -> c)
f ~ g = g . f

addio :: IO ()
addio =
    getLine >>= words ~ fmap readInt ~ sum ~ print
    -- print . sum . fmap readInt . words =<< getLine



{- SECTION: File IO -}

readFileWriteSum :: FilePath -> FilePath -> IO ()
readFileWriteSum readFromHere writeToHere = do
    -- open  read  handle
    readHandle  <- openFile readFromHere ReadMode
    -- open  write handle
    writeHandle <- openFile writeToHere  WriteMode
    -- do    stuff
    process readHandle writeHandle
    -- close write handle
    hClose writeHandle
    -- close read  handle
    hClose readHandle

process :: Handle -> Handle -> IO ()
process readHandle writeHandle = do
    isEOF <- hIsEOF readHandle
    if isEOF
    then return ()
    else do
        xss <- words <$> hGetLine readHandle
        let ys = (\xs -> read xs :: Integer) <$> xss
        hPutStrLn writeHandle (show $ sum ys)
        process readHandle writeHandle
