module LecMonad.State where
import Control.Applicative (liftA2)

-- SECTION: Labelling a tree
-- PROBLEM: Label a tree with unique Integer labels.

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

testTree :: Tree Char
testTree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

label :: Tree a -> Tree Integer
label tree = fst $ hLabel tree 0

-- hlabel :: Tree a -> Integer -> (Tree Integer, Integer) -- why tf does this error?
hLabel :: Num b => Tree a -> b -> (Tree b, b)
hLabel (Leaf x) fresh = (Leaf fresh, fresh + 1)
hLabel (Node left right) fresh =
    let (newRight, nextFresh)     = hLabel right fresh
        (newLeft,  nextNextFresh) = hLabel left nextFresh
    in
        (   Node
                newLeft
                newRight
        ,   nextNextFresh
        )

-- This solution is shit.



-- SECTION: STATE TRANSFORMERS

-- For theory. Could be any fixed type.
type State = Integer

-- Definition. State transformer (over a) is this type.
type ST' a = State -> (a, State)
newtype ST a = S (State -> (a, State)) -- needs the decorator for >> instance Monad ST where <<.

-- Generate new integers!
fresh' :: ST' Integer
fresh' state = (state, state + 1)
exampleFresh :: IO ()
exampleFresh = do
    let state = 0
        (fresh_0, state')   = fresh' state
        (fresh_1, state'')  = fresh' state'
        (fresh_2, state''') = fresh' state''
    print [fresh_0, fresh_1, fresh_2]

-- Think "apply = ($)", without the decorator >>S<<
apply :: ST a -> State -> (a, State)
apply (S st) = st

fresh :: ST Integer
fresh = S $ \state -> (state, state + 1)

-- .; Functor instance
instance Functor ST where
    fmap :: (a -> b) -> ST a -> ST b
    fmap f st = S $ \state ->
        let (x,   state') = apply st state -- run the transformer, generating an output and a new state
        in  (f x, state')                  -- change only the output, but keep the new state.

-- .; Applicative instance
instance Applicative ST where
    pure :: a -> ST a
    pure x = S (x,)
    (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = S $ \s ->
        let (f, s')    = apply stf s
            (x, s'')   = apply stx s'
        in  (f x, s'')

-- .; Monad instance
instance Monad ST where
    return :: a -> ST a
    return = pure
    (>>=)  :: ST a -> (a -> ST b) -> ST b
    stx >>= f = S $ \s ->
        let (x, s') = apply stx s   -- Generate an x and a new state s'
        in  apply (f x) s'          -- Run the state transformer >> f x << with state s'



-- SECTION: Labelling a tree with state transformers

-- Use the functor and applicative instance...
mediocreLabel :: Tree a -> ST (Tree Integer)
mediocreLabel (Leaf x) = fmap Leaf fresh
mediocreLabel (Node left right) = 
    let nodeA2 = liftA2 Node
    in  mediocreLabel left   `nodeA2`   mediocreLabel right
    -- Node <$> mediocreLabel left <*> mediocreLabel right
    -- liftA2 Node (mediocreLabel left) (mediocreLabel right)

coolLabel :: Tree a -> ST (Tree Integer)
coolLabel (Leaf x) = Leaf <$> fresh
coolLabel (Node left right) = do
    labelledLeft  <- coolLabel left
    labelledRight <- coolLabel right
    return $ Node labelledLeft labelledRight

