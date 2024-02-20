{-
Bezout's lemma solver, built for MATH2301 tute bc I don't wanna 
solve it by hand

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
main = nice



{- SECTION: IO interaction -}
outCursor :: String
outCursor = "out> "
inCursor :: String
inCursor = "in>  "

speak :: String -> IO ()
speak = putStrLn . (outCursor ++)

request :: String -> IO String
request prompt = do
    speak prompt
    putStr inCursor
    hFlush stdout
    getLine

-- Helper: Receive a, b, c
getEquation :: IO (Int, Int, Int)
getEquation = do
    speak "We will solve the equation   ax + by = c   for integers x and y, where..."
    a' <- request "a = ..."
    b' <- request "b = ..."
    c' <- request "c = ..."
    let a = abs (read a' :: Int)
        b = abs (read b' :: Int)
        c = abs (read c' :: Int)
    speak $ "Your equation is " ++ show a ++ "x + " ++ show b ++ "y = " ++ show c
    return (a, b, c)

-- Nicely formatted
-- BUG: If numbers get 
nice :: IO ()
nice = do
    -- Initialisation
    (a, b, c) <- getEquation
    -- Edge case: one of the coefficients is the gcd (super duper easy)
    let theGcd = gcd a b
    if a == theGcd || b == theGcd then do
            niceTrivialSolution (a, b, c, theGcd)
    else do
        -- Euclidean algorithm trace
        let trace = euclidTrace a b
            gcdSol =        gcdSolution trace
            problemSol =    solution gcdSol c
            hasSolution =   problemSol /= MaybeSolution Nothing
            solutionNot =   if hasSolution then "" else "not "
        if not hasSolution then do
            niceNoSolution trace
        else do
            niceNonTrivialSolution trace (a, b, c, theGcd)
    return ()

-- No solution in nice formatting
niceNoSolution :: [EuclidStep] -> IO ()
niceNoSolution trace = do
    speak $ "The Euclidean algorithm progresses like follows: \n" ++ show trace
    speak   "The equation does not have a solution!"
    return ()

-- Trivial solution in nice formatting
niceTrivialSolution :: (Int, Int, Int, Int) -> IO ()
niceTrivialSolution (a, b, c, theGcd) = do
    let coefficientWhichIsGcd = 
            if theGcd == a  then "a = " ++ show a 
                            else "b = " ++ show b
        gcdSol = 
            if theGcd == a  then FinalEquation (theGcd, a, 1, b, 0) -- "gcd = a * 1 + b * 0"
                            else FinalEquation (theGcd, a, 0, b, 1)
        sol = solution gcdSol c
    speak $ show theGcd ++ " is the coefficient " ++ coefficientWhichIsGcd ++ ", so..."
    speak $ "For the equation " ++ show a ++ "x + " ++ show b ++ "y = " ++ show theGcd ++ " we have:\n" ++ show gcdSol
    speak $ "For the equation you actually asked about, we have:\n" ++ show sol
    return ()

-- Nontrivial solution in nice formatting
-- niceNonTrivialSolution trace (a, b, c, theGcd)
niceNonTrivialSolution :: [EuclidStep] -> (Int, Int, Int, Int) -> IO ()
niceNonTrivialSolution trace (a, b, c, theGcd) = do
    let extraInfo = getInformationEquations trace
        working =   workThrough trace
        gcdSol =    gcdSolution trace
        sol =       solution gcdSol c
    let traceStrings =      map show trace
        extraStrings =      "" : "" : map show extraInfo
        workingStrings =    "" : map show working
        presentation =      ("\tEuclidean algo\t\t\t\t\tUseful equation\t\t\t\t\t\tExtended algo" :) $ 
            reverse $ zipWith3
                (\traceString extraString workingString ->
                    let extraString' =      if extraString == "" then "\t(irrelevant)" ++ replicate 4 '\t' else extraString
                        workingString' =    if workingString == "" then "\t(irrelevant)" else workingString
                    in  traceString ++ "\t<=>" ++ extraString'  ++ "\t=>" ++ workingString'
                ) 
                traceStrings extraStrings workingStrings
    speak "The extended Euclidean algorithm progresses like follows: \n"
    mapM_ putStrLn presentation
    speak $ "For the equation " ++ show a ++ "x + " ++ show b ++ "y = " ++ show theGcd ++ " we have:\n" ++ show gcdSol
    speak $ "For the equation you actually asked about, we have:\n" ++ show sol
    return ()

-- Not so nicely formatted working
notNice :: IO ()
notNice = do
    -- Initialisation
    (a, b, c) <- getEquation
    -- Euclidean algorithm trace
    let trace = euclidTrace a b
    speak $ "The Euclidean algorithm progresses like follows: \n" ++ showListLn trace
    let theGcd =        gcd a b
        gcdSol =        gcdSolution trace
        problemSol =    solution gcdSol c
        hasSolution =   problemSol /= MaybeSolution Nothing
        solutionNot =   if hasSolution then "" else "not "
    speak $ "The gcd of a and b is " ++ show theGcd
    speak $ "The equation does " ++ solutionNot ++ "have a solution!"
    if not hasSolution then do
        return ()
    else do
        let baseEquation =  getBaseEvolvingEquation trace
            extraInfo =     getInformationEquations trace
            working =       workThrough trace
            gcdSol =        gcdSolution trace
            sol =           solution gcdSol c
        -- Extended Euclidean algorithm
        speak $ "The base equation is:\n" ++ show baseEquation
        speak $ "The extra information we can use is as follows: \n" ++ showListLn extraInfo
        speak $ "Our working is: \n" ++ showListLn working
        speak $ "For the equation " ++ show a ++ "x + " ++ show b ++ "y = " ++ show theGcd ++ " we have:\n" ++ show gcdSol
        speak $ "For the equation you actually asked about, we have:\n" ++ show sol
        return ()




{- SECTION: Euclidean algorithm trace -}
type Big = Int
type CoefficientQuotient = Int
type Small = Int
type Remainder = Int
newtype EuclidStep = EuclidStep (Big, CoefficientQuotient, Small, Remainder)

instance Show EuclidStep where
    show :: EuclidStep -> String
    show (EuclidStep (b, c, s, r)) = "\t" ++ show b ++ "\t= " ++ show c ++ "\t* " ++ show s ++ "\t\t+ " ++ show r-- ++ "\n"

-- show with new line
showLn :: Show a => a -> String
showLn = (++ "\n") . show

-- show list with new lines
showListLn :: Show a => [a] -> String
showListLn = foldl1 (++) . map showLn

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



{- SECTION: Extended Euclidean algo -}
-- EvolvingEquation (g, p, y, a, z) represents the equation
-- "g = p * y - a * z"
newtype EvolvingEquation = EvolvingEquation (Int, Int, Int, Int, Int)
instance Show EvolvingEquation where
    show :: EvolvingEquation -> String
    show (EvolvingEquation (g, p, y, a, z)) = "\t" ++ show g ++ "\t= " ++ show p ++ "\t* " ++ show y ++ "\t-\t" ++ show a ++ "\t* " ++ show z-- ++ "\n"

-- InformationEquation (z, x, b, y) represents the equation
-- "z = x - b * y"
newtype InformationEquation = InformationEquation (Int, Int, Int, Int)
instance Show InformationEquation where
    show :: InformationEquation -> String
    show (InformationEquation (z, x, b, y)) = "\t" ++ show z ++ "\t= " ++ show x ++ "\t\t-\t" ++ show b ++ "\t* " ++ show y-- ++ "\n"

-- FinalEquation (g, p, y, b, z) represents the equation
-- "g = p * y + b * z"
-- A conversion b = -a converts from EvolvingEquation to FinalEquation
newtype FinalEquation = FinalEquation (Int, Int, Int, Int, Int)
instance Show FinalEquation where
    show :: FinalEquation -> String
    show (FinalEquation (g, p, y, b, z)) =
        "\t" ++ show g ++ "\t= " ++ show p ++ "\t* " ++ show y ++ "\t+\t" ++ show b ++ "\t* " ++ show z ++ "\n" ++
        "cf.\t" ++ show g ++ "\t= " ++ "x" ++ "\t* " ++ show y ++ "\t+\t" ++ "y"    ++ "\t* " ++ show z ++ "\n" ++
        "=>\t" ++ "(x, y) \t= (" ++ show p ++ ", " ++ show b ++ ")"

evolvingEquationToFinalEquation :: EvolvingEquation -> FinalEquation
evolvingEquationToFinalEquation (EvolvingEquation (g, p, y, a, z)) =
    FinalEquation (g, p, y, -a, z)

-- Evolve an EvolvingEquation
-- >> evolve ee ie << returns an EvolvingEquation which uses ie to introduce a new variable and kill one from ee.
-- Assumes that ee and ie are compatible
evolve :: EvolvingEquation -> InformationEquation -> EvolvingEquation
evolve (EvolvingEquation (g, p, y, a, z)) (InformationEquation (z', x, b, y'))
    | z /= z' || y /= y' =  error "this fucked up"
    | otherwise =           EvolvingEquation (g, -a, x, -p - a*b, y)

-- Grab base EvolvingEquation from a Euclid trace
-- yeah, this is just the second element
-- BUG: we get exactly 1 step for equation ax + by = c where gcd(a,b) is a or is b.
--      (this corresponds to a trivial equation, but it is an edge case...)
getBaseEvolvingEquation :: [EuclidStep] -> EvolvingEquation
getBaseEvolvingEquation [] =    error "0 is too few steps!"
getBaseEvolvingEquation [_] =   error "1 is too few steps!"
getBaseEvolvingEquation (_:(EuclidStep (big, coefficient, small, remainder)):_) =
    EvolvingEquation (remainder, 1, big, coefficient, small)

euclidStepToInformationEquation :: EuclidStep -> InformationEquation
euclidStepToInformationEquation (EuclidStep (big, coefficient, small, remainder)) =
    InformationEquation (remainder, big, coefficient, small)

getInformationEquations :: [EuclidStep] -> [InformationEquation]
getInformationEquations = map euclidStepToInformationEquation . drop 2

workThrough :: [EuclidStep] -> [EvolvingEquation]
workThrough history =
    scanl evolve (getBaseEvolvingEquation history) (getInformationEquations history)

gcdSolution :: [EuclidStep] -> FinalEquation
gcdSolution =
    evolvingEquationToFinalEquation . last . workThrough



{- SECTION: Solve generalised Bezout equation -}
type Cee = Int
newtype MaybeSolution = MaybeSolution (Maybe FinalEquation)
instance Show MaybeSolution where
    show :: MaybeSolution -> String
    show (MaybeSolution Nothing) = "No solution!"
    show (MaybeSolution (Just finalEquation)) = show finalEquation
instance Eq MaybeSolution where
    (==) :: MaybeSolution -> MaybeSolution -> Bool
    (==) (MaybeSolution Nothing) (MaybeSolution Nothing) = True
    (==) (MaybeSolution Nothing) _ = False
    (==) _ (MaybeSolution Nothing) = False
    (==) (MaybeSolution (Just s1)) (MaybeSolution (Just s2)) = show s1 == show s2

-- >> solution finalEquation c << returns:
--      FinalEquation c = whatever blah blah    if has solution
--      Nothing                                 if doesn't
solution :: FinalEquation -> Cee -> MaybeSolution
solution (FinalEquation (g, p, y, b, z)) c
    | c `mod` gcd y z /= 0 =    MaybeSolution Nothing
    | otherwise =               MaybeSolution . Just $ FinalEquation (c, p * c `div` g, y, b * c `div` g, z)
