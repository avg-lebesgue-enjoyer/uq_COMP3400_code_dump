{- @src: https://learnyouahaskell.github.io/functors-applicative-functors-and-monoids.html#functors-redux
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}

module Applicatives where
import Prelude 
    hiding (
        Functor, fmap, (<$), ($>), (<$>),
        Applicative, pure, (<*>), liftA2,
        sequenceA,
        -- Monoid, mappend, mempty, mconcat
    )
import Data.Kind (Type)

-- .; Functor is defined by...
class Functor (f :: Type -> Type) where
    {-# MINIMAL fmap #-}

    -- .; Provided methods
    fmap    :: (a -> b) -> f a -> f b   -- Functor mapping.
    (<$)    :: a    -- x                -- Keep the 'functor state'
            -> f b  -- fy               --  of fy, but replace the
            -> f a  -- result           --  enclosed value by x.
    ($>)    :: f a -> b -> f b          -- Flipped (<$).
    (<$>)   :: (a -> b) -> f a -> f b   -- Function lifting; i.e. fmap.
    (<&>)   :: f a -> (a -> b) -> f b   -- Flipped (<$>)

    -- .; Default implementations
    (<$)  = fmap . const
    ($>)  = flip (<$)
    (<$>) = fmap
    (<&>) = flip (<$>)

    -- .; SUBJECT TO
    --  fmap id = id
    --  fmap (u . v) = fmap u . fmap v
-- .; Useful for...
-- Taking functions (a -> b) and lifting them to functions (f a -> f b).



-- .; Applicative is defined by...
class (Functor f) => Applicative (f :: Type -> Type) where
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
    
    -- .; Provided methods
    pure    :: a -> f a             -- Create a 'pure' value.
    (<*>)   :: f (a -> b)           -- Extract a function from f,
            -> f a -> f b           --  and lift that function to f.
    liftA2  :: (a -> b -> c)        -- Take a binary operation,
            -> f a -> f b -> f c    --  and lift it to f.
    (*>)    :: f a  -- u            -- Sequence actions u and v, discarding
            -> f b  -- v            --  the *value* of u.
            -> f b      
    (<*)    :: f a -> f b -> f a    -- Sequence actions the opposite way.
    
    -- .; Default implementations
    (<*>) = liftA2 id
    liftA2 g x y = g <$> x <*> y    -- Map g over x, producing a partially
                                    --  applied function g <$> x, and apply 
                                    --  g <$> x to y.
    u *> v = (id <$ u) <*> v
    u <* v = liftA2 const u v

    -- .; SUBJECT TO
    -- IDENTITY
    --  pure id <*> v               -- Unpacking a >> pure id <<
    --  = v                         --  and applying that over v
    --                              --  behaves like you'd expect.
    --
    -- COMPOSITION
    --  pure (.) <*> u <*> v <*> w  -- Unpacking a composition
    --  = u <*> (v <*> w)           --  should compose.
    --  ~~ might be helpful to picture...
    --  pure (.) <*> f x <*> f y <*> f z
    --  = f (x .) <*> f y <*> f z
    --  = f (x . y) <*> f z
    --  = f $ (x . y) z
    --  = f $ x (y z)
    --  = f x <*> f (y z)
    --  = f x <*> (f y <*> f z)
    -- 
    -- HOMOMORPHISM
    --  pure f <*> pure x           -- Unpacking & applying pure values
    --  = pure (f x)                --  should produce the expected
    --                              --  pure result.
    --
    -- INTERCHANGE
    --  u <*> pure y                -- Unpacking and applying to a pure value
    --  = pure ($ y) <*> u          --  should produce the same result as
    --                              --  unpack-applying "evaluate at the value".
    --
    -- AGREE
    --  fmap f x                    -- Mapping is the same as
    --  = pure f <*> x              --  unpacking from pure, and then mapping.
-- .; Useful for...
-- Being able to sequence and apply actions (without being so strict as to
--  be a monad).
-- .; lem. (AGREE follows from the others)
--      fmap f x = pure f <*> x
-- pf.
--  BUG: idk... \qed



-- SECTION: Applicative examples
instance Functor Maybe where
    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing =  Nothing
    fmap f (Just x) = Just $ f x
instance Applicative Maybe where
    pure :: a -> Maybe a
    pure = Just
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ =     Nothing
    _ <*> Nothing =     Nothing
    Just f <*> Just x = Just $ f x

-- Compare the following syntax.
-- g    :: a -> b -> c -> d
-- g'   :: Applicative f
--      => f a -> f b -> f c -> f d
-- g      x     y     z
-- g' <$> x <$> y <$> z
-- Yeah, an *applicative* functor is one where you can just do all
--  your function application up on the level of the functor.

justBigChungus :: Maybe String
justBigChungus = (++) <$> Just "Big " <*> Just "Chungus"

justTheBiggestChungus :: Maybe String
justTheBiggestChungus =
    (\x y z -> x ++ y ++ z)
    <$> Just "The "
    <*> Just "Biggest "
    <*> Just "Chungus"

instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap f [] = []
    fmap f (x:xs) = f x : fmap f xs
instance Applicative [] where
    pure :: a -> [a]
    pure x = [x]
    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | f <- fs, x <- xs]    -- = map (uncurry ($)) [(f, x) | f <- fs, x <- xs]
                                            -- i.e. (,) is the univeral <*> thing.

-- This is pretty useful.
sequenceA   ::  Applicative f
            =>  [f a]
            ->  f [a]
sequenceA =
    foldr
        (liftA2 (:))
        (pure [])

cartesianProduct    ::  [[a]]
                    ->  [[a]]
cartesianProduct = sequenceA


instance Functor IO where
    fmap :: (a -> b) -> IO a -> IO b
    fmap f ia = do
        a <- ia
        return $ f a
instance Applicative IO where
    pure :: a -> IO a
    pure = return
    (<*>) :: IO (a -> b) -> IO a -> IO b
    iof <*> ioa = do
        f <- iof
        a <- ioa
        return $ f a

-- then, >> sequenceA :: [IO a] -> IO [a] << is the same as >>sequence<<.



{- SECTION: newtype -}
-- newtype is helpful for partially applying in different orders. e.g.
newtype Pair1 a b = Pair1 (a, b)
newtype Pair2 b a = Pair2 (a, b)
instance Functor (Pair1 a) where
    fmap :: (b -> c) -> Pair1 a b -> Pair1 a c
    fmap f (Pair1 (x, y)) = Pair1 (x, f y)          -- f on the snd component
instance Functor (Pair2 b) where
    fmap :: (a -> a') -> Pair2 b a -> Pair2 b a'
    fmap f (Pair2 (x, y)) = Pair2 (f x, y)          -- f on the fst component



{- SECTION: Monoids -}

-- .; Monoid is defined by...
class Monoid' m where
    {-# MINIMAL mempty', mappend' #-}
    -- .; Provided methods
    mempty'  :: m                -- nullary operation
    mappend' :: m -> m -> m      -- binary  operation
    mconcat' :: [m] -> m

    -- .; Default implementations
    mconcat' = foldr mappend' mempty'

    -- .; SUBJECT TO
    -- mappend mempty x = mappend x mempty = x
    -- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-- .; Useful for...
-- Being a monoid, duh
-- PLAYS WELL WITH FOLDS.
--      >> foldr mappend mempty << makes A LOT of sense as a computation.

instance Monoid' [a] where
    mempty' :: [a]
    mempty' = []
    mappend' :: [a] -> [a] -> [a]
    mappend' = (++)
    -- mconcat = concat

-- Bool monoids Any (False, ||) and All (True, &&).
-- resp., mconcat = any, and mconcat = all.

-- Functors send monoids to monoids. For instance,
instance Monoid' m => Monoid' (Maybe m) where
    mempty'  :: Maybe m
    mempty' = Nothing
    mappend' :: Maybe m -> Maybe m -> Maybe m
    Nothing `mappend'` y =           y
    x `mappend'` Nothing =           x
    (Just x) `mappend'` (Just y) =   Just $ x `mappend'` y

data Tree a = Empty | Node (Tree a) a (Tree a)
instance Foldable Tree where
    -- Foldable has # MINIMAL (foldr | foldMap)
    -- foldMap is conceptually really easy to implement...
    foldMap ::  Monoid m    -- Given...
            => (a -> m)     --  f :: a -> m
            -> Tree a       --  t :: Tree a
            -> m            -- ...how do we reduce t using f to 
                            --    get a result :: m?
    foldMap _ Empty = mempty
    foldMap f (Node left x right) =
        foldMap f left
        `mappend` f x
        `mappend` foldMap f right
