{-# LANGUAGE GADTs #-}

module Intro (Expression, defaultParser, simplify) where

import Prelude hiding (lex)

data Token where
  VarT :: String -> Token
  PlusT :: Token
  MulT :: Token
  LParenT :: Token
  RParenT :: Token
  ConstT :: String -> Token

instance Show Token where
  show (VarT s) = s
  show (ConstT s) = s
  show PlusT = "+"
  show MulT = "*"
  show LParenT = "("
  show RParenT = ")"


matches :: String -> Char -> Bool
matches s c = c `elem` s

symbolic :: Char -> Bool
symbolic = matches "~'!@#$%^&*-+=|\\:;<>.?/"

punctation :: Char -> Bool
punctation = matches "(){}[],"

space :: Char -> Bool
space = matches " \t\n\r"

numeric :: Char -> Bool
numeric = matches ['0'..'9']

alphanumeric :: Char -> Bool
alphanumeric = matches $ ['a'..'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['\'', '_']

lexWhile :: (a -> Bool) -> [a] -> ([a], [a])
lexWhile = span

lex :: String -> [Token]
lex inp = f . snd $ lexWhile space inp
  where f (c:cs)
          | symbolic c = let (tok, rest) = lexWhile symbolic cs in
              getSym (c:tok):lex rest
          | punctation c = let (tok, rest) = lexWhile punctation cs in
              getPunc (c:tok):lex rest
          | numeric c = let (tok, rest) = lexWhile numeric cs in
              ConstT (c:tok):lex rest
          | alphanumeric c = let (tok, rest) = lexWhile alphanumeric cs in
              VarT (c:tok):lex rest
          | otherwise = error ("unrecognised input: " ++ c:cs)
        f [] = []
        getPunc "(" = LParenT
        getPunc ")" = RParenT
        getPunc s = error ("undefined punctation: " ++ s)
        getSym "+" = PlusT
        getSym "*" = MulT
        getSym s = error ("undefined symbol: " ++ s)

data Expression  where
  Var :: String -> Expression
  Const :: Int -> Expression
  Add :: Expression -> Expression -> Expression
  Mul :: Expression -> Expression -> Expression

instance Show Expression where
  show = showPriority (0 :: Integer)
    where
      showPriority _ (Var s) = s
      showPriority _ (Const i) = show i
      showPriority pr (Add e1 e2) =
        let s = showPriority 3 e1 ++ " + " ++ showPriority 2 e2
         in if pr > 2 then "(" ++ s ++ ")" else s
      showPriority pr (Mul e1 e2) =
        let s = showPriority 5 e1 ++ " * " ++ showPriority 4 e2
         in if pr > 4 then "(" ++ s ++ ")" else s

simplify1 :: Expression -> Expression
simplify1 (Add (Const m) (Const n)) = Const (m + n)
simplify1 (Add (Const 0) x) = x
simplify1 (Add x (Const 0)) = x
simplify1 (Mul (Const m) (Const n)) = Const (m * n)
simplify1 (Mul (Const 1) x) = x
simplify1 (Mul x (Const 1)) = x
simplify1 (Mul (Const 0) _) = Const 0
simplify1 (Mul _ (Const 0)) = Const 0
simplify1 x = x

simplify :: Expression -> Expression
simplify (Add a b) = simplify1 $ Add (simplify a) (simplify b)
simplify (Mul a b) = simplify1 $ Mul (simplify a) (simplify b)
simplify x = simplify1 x




-- Grammar:
-- expression -> product | product + expression
-- product -> atom | atom * product | atom product
-- atom -> (expression) | constant | variable

parseExpression :: [Token] -> (Expression, [Token])
parseExpression toks = f $ parseProduct toks
  where f (e1, PlusT:rest) = let (e2, rest') = parseExpression rest in (Add e1 e2, rest')
        f (e1, rest) = (e1, rest)

parseProduct :: [Token] -> (Expression, [Token])
parseProduct toks = f $ parseAtom toks
  where f (e1, MulT:rest) = parseRest e1 rest
        f (e1, rest@(LParenT:_)) = parseRest e1 rest
        f (e1, rest@(ConstT _:_)) = parseRest e1 rest
        f (e1, rest@(VarT _:_)) = parseRest e1 rest
        f (e1, rest) = (e1, rest)
        parseRest e1 rest = let (e2, rest') = parseProduct rest in (Mul e1 e2, rest')

parseAtom :: [Token] -> (Expression, [Token])
parseAtom (LParenT:rest) = f $ parseExpression rest
  where f (e1, RParenT:rest') = (e1, rest')
        f _ = error "mismatching parantheses"
parseAtom ((ConstT s):toks) = (Const $ read s, toks)
parseAtom ((VarT s):toks) = (Var s, toks)
parseAtom t = error ("invalid token " ++ show t)

makeParser :: ([Token] -> (Expression, [Token])) -> String -> Expression
makeParser pfn s = let (e, rest) = (pfn . lex) s in f e rest
  where f e [] = e
        f _ rest = error ("unparsed input: " ++ show rest)

defaultParser :: String -> Expression
defaultParser = makeParser parseExpression
