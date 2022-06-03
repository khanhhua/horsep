module Horsep where

data Morse
    = MorseDot
    | MorseDash
    | MorseChar Char
    | MorseInt Int
  deriving (Show, Eq)

newtype Parse a = Parse {
    run :: String -> (a, String)
}

type MorseParser = Parse Morse

decodeDot :: MorseParser
decodeDot = Parse (\('.':xs) -> (MorseDot, xs))

decodeDash :: MorseParser
decodeDash = Parse (\('-':xs) -> (MorseDash, xs))
