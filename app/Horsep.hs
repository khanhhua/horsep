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
  | MorseChar Char
  | MorseInt Int
  | MorseEof
  | MorseEmpty
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
    f (_:xs) =Nothing

decodeSpace :: MorseParser
decodeSpace = Parse f
  where
    f [] = Nothing
    f (' ':xs) = Just (MorseSpace, xs)
    f (_:xs) = Nothing

decodeToken :: MorseParser
decodeToken = decodeDot <|> decodeDash

decodeLetter :: MorseParser
decodeLetter = f <$> many decodeToken <* decodeEof
  where
    f [MorseDot, MorseDash] = MorseChar 'A'
    f [MorseDash, MorseDot, MorseDot, MorseDot] = MorseChar 'B'
    f [MorseDash, MorseDot, MorseDash, MorseDot] = MorseChar 'C'
    f [MorseDash, MorseDot, MorseDot] = MorseChar 'D'
    f [MorseDot] = MorseChar 'E'
    f [MorseDot, MorseDot, MorseDash, MorseDot] = MorseChar 'F'
    f _ = MorseEmpty
