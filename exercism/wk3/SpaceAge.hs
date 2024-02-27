module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / (orbitalPeriods !! planetIndex planet)

-- index in orbitalPeriods
planetIndex :: Planet -> Int
planetIndex Mercury =   0
planetIndex Venus =     1
planetIndex Earth =     2
planetIndex Mars =      3
planetIndex Jupiter =   4
planetIndex Saturn =    5
planetIndex Uranus =    6
planetIndex Neptune =   7

-- orbital periods, in seconds.
orbitalPeriods :: [Float]
orbitalPeriods = map (* (365.25 * 24 * 60 * 60))
    [   0.2408467,
        0.61519726,
        1.0,
        1.8808158,
        11.862615,
        29.447498,
        84.016846,
        164.79132
    ]
