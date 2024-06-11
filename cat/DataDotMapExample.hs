module DataDotMapExample where

import qualified Data.Map as M
import Text.Read (readMaybe)
import qualified Data.Map.Internal.Debug as M.Internal.Debug

{- SECTION: Data structures -}
data Command = Quit | Empty | Insert | Delete | Lookup | ShowTree deriving (Eq, Show)
instance Read Command where
    readsPrec :: Int -> ReadS Command
    readsPrec _ thing
        | thing == cQuit = [(Quit, "")]
        | thing == cEmpty = [(Empty, "")]
        | thing == cInsert = [(Insert, "")]
        | thing == cDelete = [(Delete, "")]
        | thing == cLookup = [(Lookup, "")]
        | thing == cShowTree = [(ShowTree, "")]
        | otherwise = []

data FakeWorld = Matrix { theMap :: M.Map Integer String
                        , wakeUp :: Bool
                        }



{- LAUNCH -}
main :: IO ()
main = do
    greet
    loop $ Matrix { theMap = M.empty, wakeUp = False }

loop :: FakeWorld -> IO ()
loop world = do
    words <- fmap words getLine
    if null words then fuckOff ()
    else do
        let command = (readMaybe @Command) . head $ words
            rest    = tail words
        case command of
            Nothing -> fuckOff ()
            Just c  -> do
                world' <- dispatch world c rest
                if wakeUp world' then return () else loop world'


{- SECTION: IO helpers -}
commands :: [String]
cQuit, cEmpty, cInsert, cDelete, cLookup, cShowTree :: String
commands = [cQuit, cEmpty, cInsert, cDelete, cLookup, cShowTree]
cQuit = "die"
cEmpty = "isEmpty"
cInsert = "insert"
cDelete = "delete"
cLookup = "lookup"
cShowTree = "showTree"

dickhead :: String
dickhead = "Don't be a dickhead"
fuckOff :: a -> IO a
fuckOff a = putStrLn dickhead >> return a

done :: String
done = "Ok :)"
didIt :: a -> IO a
didIt a = putStrLn done >> return a

greet :: IO ()
greet = do
    putStrLn "Please enter one of the following: "
    mapM_ putStrLn . fmap ("  " ++) $ commands

dispatch    :: FakeWorld    -- map
            -> Command      -- command
            -> [String]     -- arguments for command
            -> IO FakeWorld -- resulting world
-- Quit
dispatch world Quit _ = return $ Matrix { theMap = M.empty, wakeUp = True }
-- Empty
dispatch world Empty _ =
    let message = if M.null . theMap $ world
                  then "It's empty yo!"
                  else "Big chungus"
    in putStrLn message >> return world
-- Insert on valid number of args
dispatch world Insert [key, value] =
    let theKey = (readMaybe @Integer) key
    in  case theKey of
        Nothing   -> fuckOff () >> return world -- Invalid key parse
        Just key' -> do                         -- Valid   key parse
            didIt ()
            return $ Matrix
                { theMap = M.insert key' value . theMap $ world
                , wakeUp = wakeUp world
                }
-- Insert on invalid number of args
dispatch world Insert _ = fuckOff () >> return world
-- Delete on valid number of args
dispatch world Delete [key] =
    withGoodKey world key
        (\w -> fuckOff () >> return w)
        (\w k -> do
            didIt ()
            return $ Matrix
                { theMap = M.delete k . theMap $ w
                , wakeUp = wakeUp w
                }
        )
{-
    let theKey = (readMaybe @Integer) key
    in  case theKey of
        Nothing   -> fuckOff () >> return world -- Invalid key parse
        Just key' -> do                         -- Valid   key parse
            didIt()
            return $ Matrix
                { theMap = M.delete key' . theMap $ world
                , wakeUp = wakeUp world
                }
-}
-- Delete on invalid number of args
dispatch world Delete _ = fuckOff () >> return world
-- Lookup on valid number of args
dispatch world Lookup [key] =
    withGoodKey world key
        (\w -> fuckOff () >> return w)
        (\w k -> do
            case M.lookup k (theMap w) of
                Nothing -> putStrLn "Key not present" >> return w
                Just v  -> print v >> return w
        )
-- Lookup on invalid number of args
dispatch world Lookup _ = fuckOff () >> return world
-- ShowTree
dispatch world ShowTree _ = do
    putStrLn . M.Internal.Debug.showTree . theMap $ world
    return world

-- Parse key, and do something.
withGoodKey :: FakeWorld                                -- world
            -> String                                   -- potentially invalid-parse key
            -> (FakeWorld -> IO FakeWorld)              -- do on invalid parse
            -> (FakeWorld -> Integer -> IO FakeWorld)   -- do on valid parse
            -> IO FakeWorld                             -- result
withGoodKey world key invalid valid =
    let theKey = (readMaybe @Integer) key
    in  case theKey of
        Nothing -> invalid world
        Just key -> valid world key
