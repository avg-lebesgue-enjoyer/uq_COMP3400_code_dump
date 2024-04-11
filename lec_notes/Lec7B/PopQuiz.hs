pythags, pythags' :: [(Integer, Integer, Integer)]
-- NOTE: This is wrong! It loops forever!
pythags = take 100 $ 
    [ (a, b, c)
        |   a <- [1..], b <- [1..], c <- [1..],
            a^2 + b^2 == c^2
    ]
-- This is good:
pythags' = take 100 $ 
    [   (a, b, c)
        |   c <- [1..],
            a <- [1..c],
            b <- [1..c],
            a^2 + b^2 == c^2
    ]


-- Define a witness f :: (a -> a -> a) -> a -> a
-- with that PRINCIPAL type
-- f need not be a total function.
-- f :: (a -> a -> a) -> a -> a
-- NOTE: This is wrong!
-- f _ a = a
f a2a2a a =
    let b = a2a2a a a
        bIsOfTypeA = a2a2a b b
    in b

{-
Beta normal form pls.
\x. x (\y. y) x
-> \x. x ((y) [y := x]) -- NOTE: NO!!!! Lazy evaluation!!! You can't evaluate through x.
-}
