{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Horsep where
import Data.Maybe
import Control.Applicative
import Data.Type.Bool (Not)


data Morse
  = MorseDot
  | MorseDash
  | MorseSpace
  | MorseEof
  deriving (Show, Eq)

data MorseAlphabet
  = MorseChar Char
  | MorseInt Int
  | MorseInvalid
  | MorseEos
  | MorseSeparator
  deriving (Show, Eq)

newtype MorseWord = MorseWord String
  deriving (Show, Eq)

newtype Parser a =
  Parse { run :: String -> Maybe (a, String) }

instance Functor Parser where
  -- Destructure the internal of Parser
  fmap f (Parse p) = Parse $ \s ->
    case p s of
      Nothing -> Nothing
      Just (morseM, str) -> Just (f morseM, str)

instance Applicative Parser where
  pure x = Parse $ Just . (x, )
  (Parse p1) <*> (Parse p2) = Parse $ \s -> do
    (f, xs1) <- p1 s
    (m, xs2) <- p2 xs1
    Just (f m, xs2)

instance Alternative Parser where
  empty = Parse $ const Nothing
  (Parse p1) <|> (Parse p2) = Parse $ \s ->
    p1 s <|> p2 s

type MorseParser = Parser Morse

type AlphabetParser = Parser MorseAlphabet

type WordParser = Parser MorseWord

decodeEof :: MorseParser
decodeEof = Parse f
  where
    f [] = Just (MorseEof, "")
    f _  = Nothing

decodeDot :: MorseParser
decodeDot = Parse f
  where
    f [] = Nothing
    f ('.':xs) = Just (MorseDot, xs)
    f (_:xs) = Nothing

decodeDash :: MorseParser
decodeDash = Parse f
  where
    f [] = Nothing
    f ('-':xs) = Just (MorseDash, xs)
    f (_:xs) = Nothing

decodeSpace :: MorseParser
decodeSpace = Parse f
  where
    f [] = Nothing
    f (' ':' ':' ':xs) = Just (MorseSpace, xs)
    f (_:xs) = Nothing

decodeToken :: MorseParser
decodeToken = decodeDot <|> decodeDash

decodeLetter :: AlphabetParser
decodeLetter = f <$> many decodeToken <* (decodeEof <|> decodeSpace)
  where
    f [MorseDot, MorseDash] = MorseChar 'A'
    f [MorseDash, MorseDot, MorseDot, MorseDot] = MorseChar 'B'
    f [MorseDash, MorseDot, MorseDash, MorseDot] = MorseChar 'C'
    f [MorseDash, MorseDot, MorseDot] = MorseChar 'D'
    f [MorseDot] = MorseChar 'E'
    f [MorseDot, MorseDot, MorseDash, MorseDot] = MorseChar 'F'
    f [MorseDash, MorseDash, MorseDot] = MorseChar 'G'
    f [MorseDot, MorseDot, MorseDot, MorseDot] = MorseChar 'H'
    f [MorseDot, MorseDot] = MorseChar 'I'
    f [MorseDot, MorseDash, MorseDash, MorseDash] = MorseChar 'J'
    f [MorseDash, MorseDot, MorseDash] = MorseChar 'K'
    f [MorseDot, MorseDash, MorseDot, MorseDot] = MorseChar 'L'
    f [MorseDash, MorseDash] = MorseChar 'M'
    f [MorseDash, MorseDot] = MorseChar 'N'
    f [MorseDash, MorseDash, MorseDash] = MorseChar 'O'
    f [MorseDot, MorseDash, MorseDash, MorseDot] = MorseChar 'P'
    f [MorseDash, MorseDash, MorseDot, MorseDash] = MorseChar 'Q'
    f [MorseDot, MorseDash, MorseDot] = MorseChar 'R'
    f [MorseDot, MorseDot, MorseDot] = MorseChar 'S'
    f [MorseDash] = MorseChar 'T'
    f [MorseDot, MorseDot, MorseDash] = MorseChar 'U'
    f [MorseDot, MorseDot, MorseDot, MorseDash] = MorseChar 'V'
    f [MorseDot, MorseDash, MorseDash] = MorseChar 'W'
    f [MorseDash, MorseDot, MorseDot, MorseDash] = MorseChar 'X'
    f [MorseDash, MorseDot, MorseDash, MorseDash] = MorseChar 'Y'
    f [MorseDash, MorseDash, MorseDot, MorseDot] = MorseChar 'Z'

    f [MorseDash, MorseDash, MorseDash, MorseDash, MorseDash] = MorseInt 0
    f [MorseDot, MorseDash, MorseDash, MorseDash, MorseDash] = MorseInt 1
    f [MorseDot, MorseDot, MorseDash, MorseDash, MorseDash] = MorseInt 2
    f [MorseDot, MorseDot, MorseDot, MorseDash, MorseDash] = MorseInt 3
    f [MorseDot, MorseDot, MorseDot, MorseDot, MorseDash] = MorseInt 4
    f [MorseDot, MorseDot, MorseDot, MorseDot, MorseDot] = MorseInt 5
    f [MorseDash, MorseDot, MorseDot, MorseDot, MorseDot] = MorseInt 6
    f [MorseDash, MorseDash, MorseDot, MorseDot, MorseDot] = MorseInt 7
    f [MorseDash, MorseDash, MorseDash, MorseDot, MorseDot] = MorseInt 8
    f [MorseDash, MorseDash, MorseDash, MorseDash, MorseDot] = MorseInt 9

    f _ = MorseInvalid

decodeWord :: WordParser
decodeWord = f <$> many decodeLetter <* decodeEos
  where
    decodeEos = MorseEos <$ decodeEof
    -- decodeWordSep = MorseSeparator <$ decodeSpace
    f [] = MorseWord ""
    f chars = foldl f' (MorseWord "") chars
    -- Where: ‘a’ is a rigid type variable bound by the inferred type of f :: [a] -> MorseWord at /Users/khanhhua/dev/horsep/app/Horsep.hs:(132,5)-(133,44)
    f' :: MorseWord -> MorseAlphabet -> MorseWord
    f' (MorseWord word) (MorseChar c) = MorseWord $ word ++ [c]
    f' morseWord _ = morseWord
