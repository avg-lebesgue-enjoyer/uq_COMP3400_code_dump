{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Gaming where

data One = Zero

instance Eq One where
    (==) :: One -> One -> Bool
    Zero == Zero  =  Zero == Zero

instance Num One where
    fromInteger :: Integer -> One
    fromInteger = const Zero
    -- idc about the rest below this
    (+) :: One -> One -> One
    (+) = undefined
    (*) :: One -> One -> One
    (*) = undefined
    abs :: One -> One
    abs = undefined
    signum :: One -> One
    signum = undefined
    negate :: One -> One
    negate = undefined

tester :: One -> IO ()
tester 0 = putStrLn "Success!"
tester _ = putStrLn "Completeness"




syntacticSugar :: (Num a, Eq a) => Maybe a -> String
--             :: forall a. ...
syntacticSugar (Just 0) =
-- argument before = desugars to >> \argument -> <<
-- Pattern match in function definition desugars to >> case ... of <<
-- Pattern match against numeric literal desugars to >> (==) << comparison
-- >> 0 :: forall a. Num a => a << itself desugars to >> fromIntegral (0 :: Integer) <<
    do
    -- do itself
        x <- "abc"
        -- >> x <- e << desugars to >> e >>= \x -> ... <<
        -- >> "abc" << desugars to >> 'a':'b':'c':[] <<
        y <- [0, 1, 2]
        -- desugars to >> 0:1:2:[] <<
        z <- [ (\u v -> 'a') | char <- ['a' .. 'z'], char > 'a']
        -- >> \ u v -> ... << desugars to >> \ u -> \ v -> ... <<
        -- List comprehension desugars to an expression involving concatMap, singletons 
        let (x', y') = (x, y)
        -- let inside do desugars to >> let ... in ... << outside do
        -- (x, y) desugars to (,) x y
        (x `return`)
        -- infix with `backticks` desugars to prefix
        -- sections of infix operators desugar to lambda expressions
syntacticSugar (Just x)
    | x /= -0  =  
    -- guards desugar to a chain of >> if ... then ... else ... << expressions
    -- unary minus desugars to >> negate <<
        if True then "yes" else "nope"
        -- >> if b then t else e << desugars to >> \b -> case b of { True -> t ; False -> e }

-- Comments are syntactic sugar for whitespace

-- Haskell is syntactic sugar for the compiled C code
-- The compiled C code is syntactic sugar for the assembled Assembly code
-- The assembled Assembly code is syntactic sugar for machine code
