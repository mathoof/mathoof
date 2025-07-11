{-# LANGUAGE GADTs #-}

module Parser
  ( Parser (..),
    parse,
    charP,
    stringP,
    spanP,
    notNullP,
    wsP,
  )
where

import Control.Applicative
import Data.Char

newtype Parser a where
    Parser :: {runParser :: String -> Maybe (String, a)} -> Parser a

instance Functor Parser where
    fmap f (Parser p) = Parser parser
      where
        parser input = do
            (input', a) <- p input
            Just (input', f a)

instance Applicative Parser where
    pure a = Parser $ \input -> Just (input, a)
    (Parser p1) <*> (Parser p2) = Parser f
      where
        f input = do
            (input', fab) <- p1 input
            (input'', a') <- p2 input'
            Just (input'', fab a')

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser f
      where
        f inp = p1 inp <|> p2 inp

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (c : cs) | c == x = Just (cs, c)
    f _ = Nothing

stringP :: String -> Parser String
stringP = traverse charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \inp ->
    let (token, rest) = span f inp
     in Just (rest, token)

notNullP :: Parser [a] -> Parser [a]
notNullP (Parser p) = Parser f
  where
    f input = do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

wsP :: Parser String
wsP = spanP isSpace

parse :: Parser a -> String -> a
parse parser input = f $ runParser parser input
  where f (Just b) = case b of
          ([], c) -> c
          (_:_, _) -> error "didn't parse all of input"
        f Nothing = error "program failed"
