{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Horsep where
import Data.Maybe
import Control.Applicative


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

type AlphabetParser = Parser (Maybe MorseAlphabet)

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
decodeLetter = f <$> some decodeToken <* (decodeEof <|> decodeSpace)
  where
    f [MorseDot, MorseDash] = Just $ MorseChar 'A'
    f [MorseDash, MorseDot, MorseDot, MorseDot] = Just $ MorseChar 'B'
    f [MorseDash, MorseDot, MorseDash, MorseDot] = Just $ MorseChar 'C'
    f [MorseDash, MorseDot, MorseDot] = Just $ MorseChar 'D'
    f [MorseDot] = Just $ MorseChar 'E'
    f [MorseDot, MorseDot, MorseDash, MorseDot] = Just $ MorseChar 'F'
    f [MorseDash, MorseDash, MorseDot] = Just $ MorseChar 'G'
    f [MorseDot, MorseDot, MorseDot, MorseDot] = Just $ MorseChar 'H'
    f [MorseDot, MorseDot] = Just $ MorseChar 'I'
    f [MorseDot, MorseDash, MorseDash, MorseDash] = Just $ MorseChar 'J'
    f [MorseDash, MorseDot, MorseDash] = Just $ MorseChar 'K'
    f [MorseDot, MorseDash, MorseDot, MorseDot] = Just $ MorseChar 'L'
    f [MorseDash, MorseDash] = Just $ MorseChar 'M'
    f [MorseDash, MorseDot] = Just $ MorseChar 'N'
    f [MorseDash, MorseDash, MorseDash] = Just $ MorseChar 'O'
    f [MorseDot, MorseDash, MorseDash, MorseDot] = Just $ MorseChar 'P'
    f [MorseDash, MorseDash, MorseDot, MorseDash] = Just $ MorseChar 'Q'
    f [MorseDot, MorseDash, MorseDot] = Just $ MorseChar 'R'
    f [MorseDot, MorseDot, MorseDot] = Just $ MorseChar 'S'
    f [MorseDash] = Just $ MorseChar 'T'
    f [MorseDot, MorseDot, MorseDash] = Just $ MorseChar 'U'
    f [MorseDot, MorseDot, MorseDot, MorseDash] = Just $ MorseChar 'V'
    f [MorseDot, MorseDash, MorseDash] = Just $ MorseChar 'W'
    f [MorseDash, MorseDot, MorseDot, MorseDash] = Just $ MorseChar 'X'
    f [MorseDash, MorseDot, MorseDash, MorseDash] = Just $ MorseChar 'Y'
    f [MorseDash, MorseDash, MorseDot, MorseDot] = Just $ MorseChar 'Z'

    f [MorseDash, MorseDash, MorseDash, MorseDash, MorseDash] = Just $ MorseInt 0
    f [MorseDot, MorseDash, MorseDash, MorseDash, MorseDash] = Just $ MorseInt 1
    f [MorseDot, MorseDot, MorseDash, MorseDash, MorseDash] = Just $ MorseInt 2
    f [MorseDot, MorseDot, MorseDot, MorseDash, MorseDash] = Just $ MorseInt 3
    f [MorseDot, MorseDot, MorseDot, MorseDot, MorseDash] = Just $ MorseInt 4
    f [MorseDot, MorseDot, MorseDot, MorseDot, MorseDot] = Just $ MorseInt 5
    f [MorseDash, MorseDot, MorseDot, MorseDot, MorseDot] = Just $ MorseInt 6
    f [MorseDash, MorseDash, MorseDot, MorseDot, MorseDot] = Just $ MorseInt 7
    f [MorseDash, MorseDash, MorseDash, MorseDot, MorseDot] = Just $ MorseInt 8
    f [MorseDash, MorseDash, MorseDash, MorseDash, MorseDot] = Just $ MorseInt 9

    f _ = Nothing

decodeWord :: Parser MorseWord
decodeWord = f . sequence <$> some decodeLetter  -- <* decodeEos
  where
    f Nothing = MorseWord ""
    f (Just alphabets) = foldl f' (MorseWord "") alphabets
    f' (MorseWord word) (MorseChar c) = MorseWord (word ++ [c])
    f' morseWord _ = morseWord