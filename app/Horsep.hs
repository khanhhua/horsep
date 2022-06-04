{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Horsep where
import Data.Maybe
import Control.Applicative


data Morse
  = MorseDot
  | MorseDash
  | MorseSpace
  | MorseChar Char
  | MorseInt Int
  | MorseEof
  deriving (Show, Eq)

newtype Parser a =
  Parse { run :: String -> (Maybe a, String) }

instance Functor Parser where
  -- Destructure the internal of Parser
  fmap f (Parse p) = Parse $ \s ->
    let
      (morse, str) = p s
    in case morse of
      Nothing -> (Nothing, str)
      Just morseM -> (Just $ f morseM, str)

instance Applicative Parser where
  pure x = Parse (Just x, )
  (Parse p1) <*> (Parse p2) = Parse $ \s ->
    let
      (f, xs1)   = p1 s
      (m, xs2)  = p2 xs1
    in (f <*> m, xs2)

instance Alternative Parser where
  empty = Parse f
    where
      f (_:xs) = (Nothing, xs)
      f _ = (Nothing, "")
  (Parse p1) <|> (Parse p2) = Parse $ \s ->
    let
      (m1, xs1) = p1 s
      (m2, xs2) = p2 s
    in (m1 <|> m2, xs1)

type MorseParser = Parser Morse


decodeEof :: MorseParser
decodeEof = Parse f
  where
    f [] = (Just MorseEof, "")
    f (_:xs) = (Nothing, xs)

decodeDot :: MorseParser
decodeDot = Parse f
  where
    f [] = (Nothing, "")
    f ('.':xs) = (Just MorseDot, xs)
    f (_:xs) = (Nothing, xs)

decodeDash :: MorseParser
decodeDash = Parse f
  where
    f [] = (Nothing, "")
    f ('-':xs) = (Just MorseDash, xs)
    f (_:xs) = (Nothing, xs)

decodeSpace :: MorseParser
decodeSpace = Parse f
  where
    f [] = (Nothing, "")
    f (' ':xs) = (Just MorseSpace, xs)
    f (_:xs) = (Nothing, xs)

decodeToken :: MorseParser
decodeToken = decodeDot <|> decodeDash

decodeLetter ::MorseParser
decodeLetter = many decodeToken <* decodeEof
  -- where
  --   f _a = Nothing
    -- f [MorseDot, MorseDash] = MorseChar 'A'
    -- f [] = MorseSpace