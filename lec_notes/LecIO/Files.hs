module LecIO.Files where

import System.IO hiding () -- ( Handle, IOMode (..), openFile, hClose, hGetLine ) 

readFileWriteSum :: FilePath -> FilePath -> IO ()
readFileWriteSum readFromHere writeToHere = do
    -- open  read  handle
    readHandle  <- openFile readFromHere ReadMode
    -- open  write handle
    writeHandle <- openFile writeToHere  WriteMode
    -- do    stuff
    line <- hGetLine readHandle
    -- close write handle
    hClose writeHandle
    -- close read  handle
    hClose readHandle

