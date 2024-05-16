{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <$" #-}
{-# HLINT ignore "Use void" #-}
module LecParser.Parse where
import Control.Applicative ( Alternative(empty, (<|>)), Applicative (liftA2) )

{- SECTION: Parser type and instances -}

newtype Parser a = P { parse :: String -> Maybe (a, String)}
-- parse :: Parser a -> (String -> Maybe (a, String))

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= a2pb = P $ \cs ->
        case parse pa cs of
            Nothing         -> Nothing
            Just (a, cs')   -> parse (a2pb a) cs'

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f pa = do
        a <- pa
        return $ f a
    -- fmap f p = P $ \cs ->
    --     case parse p cs of
    --         Nothing         -> Nothing
    --         Just (a, s')    -> Just (f a, s')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P $ \cs -> Just (a, cs)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> pa = pf >>= (<$> pa)
    -- pf <*> pa = do
    --     f <- pf
    --     fmap f pa



{- SECTION: Simple example parsers -}

-- Empty parser (always fails when it parses)
empty' :: Parser a
empty' = P (const Nothing)

-- Parse one character
one :: Parser Char
one = P parser
    where
        parser :: String -> Maybe (Char, String)
        parser []     = Nothing                     -- Can't parse nothing
        parser (c:cs) = Just (c, cs)                -- Parse one character

-- Parse one character IF it satisfies a condition
oneIf   :: (Char -> Bool) -- condition
        -> Parser Char
oneIf condition = do
    c <- one        -- parse one character
    if condition c then
        return c    -- If c satisfies condition, keep it
    else
        empty       -- Else, fail; empty parser

-- Parse a digit as a string
digit :: Parser Char
digit = oneIf (`elem` ['0' .. '9'])



{- SECTION: Alternative -}

class Applicative f => Alternative' f where
    empty'' :: f a
    (<||>) :: f a -> f a -> f a
    -- .; Alternative laws are:
    -- (f a, empty, (<|>)) is a monoid

instance Alternative Parser where
    empty :: Parser a
    empty = empty'

    (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P $ \cs ->
        case parse p cs of
            Nothing -> parse q cs
            px      -> px          -- aka. Just (c, cs') -> Just (c, cs')



{- SECTION: More parsers -}

-- Parse ONE OR MORE times repeatedly until failure
some    :: Parser a     -- pa: base parser 
        -> Parser [a]   -- result
some pa = do
    a <- pa                     -- Try to parse a character
    as <- some pa <|> pure []   -- Try to parse the rest; in case of failure, get a parser with [a].
    return $ a : as             -- Stick it together

-- Parse ZERO OR MORE times repeatedly until failure
many    :: Parser a     -- pa : Base parser
        -> Parser [a]
many pa = some pa <|> pure []

-- >> some << and >> many << mega generalise:
some'   :: Alternative m
        => m a
        -> m [a]
some' v = v `liftCons` many' v
    where liftCons = liftA2 (:)

many'   :: Alternative m
        => m a
        -> m [a]
many' v = some' v <|> pure []

-- Parse an INTEGER
nat :: Parser Integer
nat = do
    xs <- some digit
    pure $ read xs

-- Parse a PARTICULAR CHARACTER
char    :: Char         -- particular: particular character
        -> Parser Char
char particular = oneIf (== particular)

-- Parse a PARTICULAR STRING
string  :: String       -- particular: particular string
        -> Parser String
string [] = 
    pure []
string (c:cs) = do
    c'  <- char c
    cs' <- string cs
    pure $ c' : cs'

-- Parse (many) space characters, and discard them
space :: Parser ()
space = const () <$> many (oneIf (== ' '))
-- space = do
--     many (char ' ')

-- Parse a token, discarding space on either side.
token   :: Parser a     -- pa: the parser parsing the token
        -> Parser a
token pa = do
    space
    a <- pa
    space
    pure a



{- SECTION: Converting a grammar to a Parser -}

data Expr = Value Integer | Add Expr Expr deriving Show
{-  Grammar is:
        expr   ::= factor + expr  |  factor
        factor ::= (expr)         |  value
        value  ::= 0 | 1 | 2 | ...
    The left of any "|" has precedence over things on the right.
-}

-- expr ::= factor + expr  |  factor
expr :: Parser Expr
expr = do
    f <- factor         -- factor
    token $ char '+'    -- +
    e <- expr           -- expr
    pure $ Add f e
    <|>                 -- |
    factor              -- factor

{- More efficient:
expr :: Parser Expr
expr = do
    f <- factor         -- factor
    do
        token $ char '+'    -- +
        e <- expr           -- expr
        pure $ Add f e
        <|> pure f          -- Re-use cached >>f<< factor reading.
-}

-- factor ::= (expr)  |  value
factor :: Parser Expr
factor = do
    token $ char '('    -- (
    e <- expr           -- expr
    token $ char ')'    -- )
    pure e
    <|>                 -- |
    value               -- value

-- value ::= 0 | 1 | ...
value :: Parser Expr
value = Value <$> nat



{- SECTION: +, * parser -}

data Expr' = Value' Integer | Add' Expr' Expr' | Mul' Expr' Expr' deriving Show

{-  Grammar is:
        expr ::= term + expr    |  term
        term ::= factor * term  |  factor
        factor ::= (expr)       |  val
        val ::= 0 | 1 | 2 | ...
    Left before right.
-}

expr' :: Parser Expr'
expr' = do
    t <- term'
    token $ char '+'
    e <- expr'
    pure $ Add' t e
    <|>
    term'

term' :: Parser Expr'
term' = do
    f <- factor'
    token $ char '*'
    t <- term'
    pure $ Mul' f t
    <|>
    factor'

factor' :: Parser Expr'
factor' = do
    token $ char '('
    e <- expr'
    token $ char ')'
    pure e
    <|>
    val'

val' :: Parser Expr'
val' = Value' <$> nat
