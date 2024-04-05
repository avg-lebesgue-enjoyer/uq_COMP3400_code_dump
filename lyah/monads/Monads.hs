{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use join" #-}
{-# HLINT ignore "Use foldM_" #-}
module Monads where
import Data.Kind ( Type )

{- SECTION: Monoid in the category of endofunctors -}
-- @src: https://learnyouahaskell.github.io/a-fistful-of-monads.html
-- @src: https://hackage.haskell.org/package/base-4.19.1.0/docs/Control-Monad.html#g:1
-- .; A monad is a monoid in the category of endofunctors.
class Applicative m => MonoidEndofunctor (m :: Type -> Type) where

    -- .; Provided methods
    (>>=!)   :: m a -> (a -> m b) -> m b
    (>>!)    :: m a ->  m b       -> m b
    return'  :: a   ->  m a
    -- think of (>>=) as
    --  mx >>= f
    --  = do
    --      x <- mx
    --      f x         -- auto-packs the result
    -- i.e. "unpack-mappack"

    -- think of (>>) as
    --  ma >> mb = do
    --      ma  -- result discarded, but action done
    --      mb  -- action done, result returned

    -- .; Default implementations
    return' = pure
    (>>!) = (*>)

    -- .; SUBJECT TO
    -- LEFT IDENTITY
    --  return a >>= f      -- Putting the pure value >>a<< into >>f<<
    --  = f a               --  is the same as running a through f
    -- ^^This can be rephrased as
    --  (return -) >>= f
    --  = f
    -- which makes the name more clear.
    -- or even better,
    --  return >=> f
    --  = f
    --
    -- RIGHT IDENTITY
    --  m >>= return        -- Unpacking from m and returning
    --  = m                 --  is the same as doing nothing
    -- i.e.
    --  f >=> return
    --  = f
    --
    -- ASSOCIATIVITY
    --  m >>= (\x -> f x >>= g)
    --  = (m >>= f) >>= g
    -- ^^This can be rephrased as
    --  m >>= (f - >>= g)   -- Running into "g after f"
    --  = (m >>= f) >>= g   --  is the same as running into f, and then into g.
    -- which makes the name more clear.
    -- Even clearer, leveraging
    --      (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
    --      g <=< f $ x = f x >>= g
    --  or its swapped counterpart
    --      f >=> g $ x = return x >>= f >>= g
    --  which are both just "function composition", the associativity law
    --  is associativity of (<=<):
    --  (h <=< g) <=< f
    --  = h <=< (g <=< f)
    --  or equivalently, of (>=>):
    --  f >=> (g >=> h)
    --  = (f >=> g) >=> h
    --
    -- .; RELATES TO Applicative VIA (these are required to be enforced)
    -- PURE IS RETURN
    --  pure = return
    --
    -- <*> (Unpack-unpack-map-pack) IS >>= (Unpack-mappack)
    --  mf <*> mx
    --  = mf >>= (\f -> 
    --      mx >>= (\x ->
    --          return $ f x
    --      )
    --      )
    -- .; RELATES TO Functor VIA (these are automatically ensured)
    -- fmap ("map") IS >>= return ("Unpack-map-pack")
    --  fmap f mx
    --  = mx >>= return . f
    --
    -- >> IS *>
    --  (>>) = (*>)



{- SECTION: More monads -}
-- @src: https://learnyouahaskell.github.io/for-a-few-monads-more.html

-- .; Writer
-- Control.Monad exports Control.Monad.Writer, defined like this:
newtype Writer w a = Writer { runWriter :: (a, w) }
instance Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer (a, w)) = Writer (f a, w)
instance (Monoid w) => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure a = Writer (a, mempty)
    (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    (Writer (f, wf)) <*> (Writer (a, wa)) =
        Writer (f a, wf `mappend` wa)
instance (Monoid w) => Monad (Writer w) where
    (>>=) :: Monoid w => Writer w a -> (a -> Writer w b) -> Writer w b
    (Writer (a, wa)) >>= f =
        let Writer (fa, wb) = f a
        in  Writer (fa, wa `mappend` wb)
-- It is useful for *writing logs >>::w<< onto values >>::a<< that you
--  actually care about*.

-- This is exported too:
tell :: Monoid w => w -> Writer w ()
tell message = Writer ((), message)

-- Example:
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])
logTheProduct :: Writer [String] Int
logTheProduct = do
    -- logNumber :: Int -> Writer [String] Int
    x <- logNumber 3
    y <- logNumber 5
    tell ["Took the product."]
    return $ x * y
-- inspect via >> runWriter logTheProduct <<

-- .; Reader
-- Control.Monad.Instances exports this instance
-- instance Monad ((->) r) where
--     return :: a -> (r -> a)
--     return = const
--     (>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
--     r2a >>= a2r2b = --r =
--         -- let a = r2a r
--         --     r2b = a2r2b a
--         -- in  r2b
--         a2r2b . r2a

-- It behaves like in this example:
hesitate :: String -> String
hesitate x = let
    shoutingX = (++"!") x
    reallyX =   (++"?") x
    in reallyX ++ " " ++ shoutingX
hesitate' :: String -> String
hesitate' = do          -- pretend input >>x<< is fed to >>hesitate'<<.
    shouting <- (++"!") -- grab eventual result of >> (++"!") x <<
    really <-   (++"?") -- grab eventual result of >> (++"?") x <<
    return (really ++ " " ++ shouting)

-- .; Control.Monad.State
-- "Manipulating a single data structure" isn't a real concept in Haskell.
-- But... "returning modified versions of an old structure" is.
-- Doing this^^ requires the extra overhead of 'always keeping tack of
--  the current structure'. It'd be nice if we could abstract that away
--  and just work with the stuff that we actually care about.
-- We want to "abstract away the state of something".
-- Control.Monad.State does this.

-- Here's a data structure.
type Stack = [Int]
pop     :: Stack -> (Int, Stack)
push    :: Int -> Stack -> ((), Stack)
pop (x:xs) =    (x, xs)
push x xs =     ((), x:xs)
-- We'll have one of these to carry around in some "state".

-- Control.Monad.State exports >> State <<. One thinks of
--  >> State s << as a *computation* (i.e. a function) that
--  manipulates a state >>::s<< into a value >>::a<< and a 
--  new state >>::s<<, packed into result >> :: (a, s) <<.
newtype State s a = State { runState :: s -> (a, s) }
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) =
        let h s =
                let (a, s') = g s   -- grab result and new state
                in  (f a, s')       -- map over result
        in  State h                 -- action of producing new state with mapped result
instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (a, ) -- >> = \s -> State (a, s) << is the "yield >>a<< and don't modify state" action
    (<*>) :: State s (a -> b) -> State s a -> State s b
    f <*> g =
        let h s =
                let (a, s') =   runState g s
                    (f', s'') = runState f s
                in  (f' a, s'')
        in  State h
instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    sh >>= f =
        let g s =
                let (a, s') = runState sh s -- run state transform sh on s
                    sk = f a                -- apply f to a, yielding state transform sk
                in  runState sk s'          -- run state transform sk on s'
        in  State g

-- To make Stack work with our state computations, we actually think of
-- push and pop as working with "the state of some stack" in mind:
pop'  :: State Stack Int
pop' = State $ \(x:xs) -> (x, xs)
push' :: Int -> State Stack ()
push' x = State $ \xs -> ((), x:xs)

-- Now we can manipulate a stack as follows.
manipulateStack :: State Stack Int
manipulateStack = do
    push' 3     -- push >>3<< onto the stack
    pop'        -- pop from the stack
    pop'        -- pop again from the stack
-- Example use: >> runState manipulateStack [5, 8, 2, 1] <<
-- See how the "background stack" is abstracted away? The result is a
--  state transformation >>manipulateStack<< which expects an "initial
--  stack state" and produces for us a tuple (final result, final stack state).

-- .; MonadState
-- Control.Monad.State exports MonadState which is
class Monad m => MonadState s m where
    get     :: m s                  -- Grab the state from inside m
    put     :: s -> m ()            -- Replace the state inside m
    state   :: (s -> (a, s)) -> m a -- Embed a simple state action into m

-- and is cool ig.

-- .; Either e
-- Data.Either exports the instance
-- instance Monad (Either e) where
--     return :: a -> Either e a
--     (>>=)  :: Either e a -> (a -> Either e b) -> Either e b

--     return = Right

--     Right x     >>= f = f x
--     Left  error >>= f = Left error



{- SECTION: Useful monadic functions -}

-- >> liftM :: Monad m => (a -> b) -> (m a -> m b) <<
--  used to be relevant. Now, we have an appropriate dependency
--  >> Functor m => Applicative m => Monad m <<
--  so we have >>fmap<< available to use on Monads. This is all
--  that liftM used to do.
-- It is worth noting though that
--  fmap f ma
--  = ma >>= (return . f)
-- which was the old implementation for >>liftM<<.

-- >> ap :: Monad m => m (a -> b) -> m a -> m b <<
--  used to exibe relevantst. Appropriate dependencies now have >>ap<< replaced
--  by >> (<*>) <<.
-- It is worth noting that
--  mf <*> ma
--  = do
--      f <- mf
--      a <- ma
--      return $ f a
-- which was the old implementation of >>ap<<.

-- .; join
-- We can "flatten" monads as follows.
join :: (Monad m) => m (m a) -> m a
join mma = do
    ma <- mma
    ma
-- Instances include
--  join (Just mx) =  mx
--  join Nothing =    Nothing
--  (join :: [[a]]) = concat
--  join (Right mx) = mx
--  join (Left e)   = Left e
-- NOTE: We have
--  m >> f
--  = join (fmap f m)
-- which makes sense if you think about it.

-- .; filterM
-- Suppose filtering by >>p<< returns some context.
-- Can we still filter, and propagate the context to the outside?
-- Yes!
filterM ::  Monad m
        =>  (a -> m Bool)   -- p
        ->  [a]             -- [] | (x:xs)
        ->  m [a]           -- result
filterM _ [] = return []
filterM p (x:xs) =
    p x >>=
        (\b ->
            if b
            then fmap (x :) (filterM p xs)
            else filterM p xs
        )

-- Here's a cool party trick:
powerSet :: [a] -> [[a]]
powerSet =                      -- To get the power set of a set...
    filterM                     -- ...filter...
        (const [True, False])   -- ...non-deterministically choosing 
                                --     to both keep and exclude.

-- Here's an actual use case
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too big; throwing it away"]
        return False
seeKeepSmall :: IO ()
seeKeepSmall = do
    putStrLn "gimme a list :: [Int]"
    theList <- getLine
    let list = read theList :: [Int]
    let smalls = filterM keepSmall list
    putStrLn $ "the result is " ++ (show . fst . runWriter $ smalls)
    putStrLn "Here's how we got there:"
    mapM_ putStrLn . snd . runWriter $ smalls
    return ()

-- .; foldM
-- foldl                           foldM
--  ::                              ::  Monad m
--      (a -> b -> a)               =>  (a -> b -> m a)
--      ->  a                       ->  a
--      ->  [b]                     ->  [b]
--      ->  a                       ->  m a
-- i.e. foldM moves context to the result of a fold.
foldM   ::  Monad m
        =>  (a -> b -> m a)
        ->  a
        ->  [b]
        ->  m a
foldM _ base [] = return base
foldM step base (b:bs) =
    step base b 
    >>= (\acc -> foldM step acc bs)

-- Here's a dumb cashier. They can only compute sums 
--  >> x + y << where >> y < 10 <<. Else, their brain
--  gets fried.
dumbSum :: Int -> Int -> Maybe Int
dumbSum acc y
    | y >= 10 =     Nothing
    | otherwise =   Just $ acc + y
seeDumbSum :: IO ()
seeDumbSum = do
    putStrLn "gimme a list :: [Int]"
    list <- getLine >>= (\theList -> return (read theList :: [Int]))
    let stoopid = foldM dumbSum 0 list
    putStrLn $ "sum by dumbass is " ++ show stoopid
    return ()

-- .; Folding with (<=<)
-- inMany n start =
--     foldr (<=<) return (replicate n moveKnight) =<< return start
-- This is monoidal fold, with monoid ((r ->), return, (<=<)).