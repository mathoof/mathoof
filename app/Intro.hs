{-# LANGUAGE GADTs #-}

module Intro (Expression (..), expression) where

import Control.Applicative
import Data.Char
import Parser

data Expression where
    Var :: String -> Expression
    Constant :: Int -> Expression
    Add :: Expression -> Expression -> Expression
    Mul :: Expression -> Expression -> Expression
    deriving (Show, Eq)

expression :: Parser Expression
expression = addP <|> productP
  where
    productP = mulStarP <|> mulP <|> atomP
    atomP = paranthesizedP <|> constantP <|> varP
    paranthesizedP = charP '(' *> wsP *> expression <* wsP <* charP ')'
    constantP = Constant . read <$> notNullP numberP
    varP = Var <$> notNullP (spanP isAlphaNum)
    numberP = spanP isDigit
    mulStarP =
        Mul
            <$> atomP
            <*> (wsP *> charP '*' *> wsP *> productP)
    mulP = Mul <$> atomP <*> (wsP *> productP)
    addP =
        Add
            <$> productP
            <*> (wsP *> charP '+' *> wsP *> expression)
