{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Main where

import Control.Applicative
import Data.Char
import qualified Data.Set as Set
import Intro
import Parser

-- Grammar:
-- formula -> imp | imp <=> formula
-- imp -> or | or => imp
-- or -> and | and \/ or
-- and -> not | not /\ and
-- not -> atom | ~ not
-- atom -> (formula) | True | False | Atom


newtype Prop = Prop String deriving (Eq, Ord)
instance Show Prop where
    show (Prop a) = a

data Formula a where
    FalseF :: Formula a
    TrueF :: Formula a
    AtomF :: a -> Formula a
    NotF :: (Formula a) -> Formula a
    AndF :: (Formula a) -> (Formula a) -> Formula a
    OrF :: (Formula a) -> (Formula a) -> Formula a
    ImpF :: (Formula a) -> (Formula a) -> Formula a
    IffF :: (Formula a) -> (Formula a) -> Formula a
    ForallF :: String -> (Formula a) -> Formula a
    ExistsF :: String -> (Formula a) -> Formula a
    deriving (Functor, Foldable, Eq, Ord)

instance Applicative Formula where
  pure = AtomF
  FalseF <*> _ = FalseF
  TrueF <*> _ = TrueF
  AtomF f <*> fA = fmap f fA
  NotF f <*> fA = NotF (f <*> fA)
  AndF f1 f2 <*> fA = AndF (f1 <*> fA) (f2 <*> fA)
  OrF f1 f2 <*> fA = OrF (f1 <*> fA) (f2 <*> fA)
  ImpF f1 f2 <*> fA = ImpF (f1 <*> fA) (f2 <*> fA)
  IffF f1 f2 <*> fA = IffF (f1 <*> fA) (f2 <*> fA)
  ForallF x f <*> fA = ForallF x (f <*> fA)
  ExistsF x f <*> fA = ExistsF x (f <*> fA)

instance Monad Formula where
  FalseF >>= _ = FalseF
  TrueF >>= _ = TrueF
  AtomF a >>= f = f a
  NotF a >>= f = NotF (a >>= f)
  AndF a1 a2 >>= f = AndF (a1 >>= f) (a2 >>= f)
  OrF a1 a2 >>= f = OrF (a1 >>= f) (a2 >>= f)
  ImpF a1 a2 >>= f = ImpF (a1 >>= f) (a2 >>= f)
  IffF a1 a2 >>= f = IffF (a1 >>= f) (a2 >>= f)
  ForallF x a >>= f = ForallF x (a >>= f)
  ExistsF x a >>= f = ExistsF x (a >>= f)


instance (Show a) => Show (Formula a) where
    show FalseF = "false"
    show TrueF = "true"
    show (AtomF a) = show a
    show (NotF a) = "~ " ++ show a
    show (AndF a b) = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
    show (OrF a b) = "(" ++ show a ++ " \\/ " ++ show b ++ ")"
    show (ImpF a b) = "(" ++ show a ++ " ==> " ++ show b ++ ")"
    show (IffF a b) = "(" ++ show a ++ " <=> " ++ show b ++ ")"
    show (ForallF a b) = "(" ++ "forall " ++ show a ++ " : " ++ show b ++ ")"
    show (ExistsF a b) = "(" ++ "exists " ++ show a ++ " : " ++ show b ++ ")"

formula :: Parser (Formula Prop)
formula =
    IffF <$> impP <*> (wsP *> stringP "<=>" *> wsP *> formula) <|> impP
  where
    impP = ImpF <$> orP <*> (wsP *> stringP "==>" *> wsP *> impP) <|> orP
    orP = OrF <$> andP <*> (wsP *> stringP "\\/" *> wsP *> orP) <|> andP
    andP = AndF <$> notP <*> (wsP *> stringP "/\\" *> wsP *> andP) <|> notP
    notP = charP '~' *> wsP *> (NotF <$> notP) <|> atomP
    atomP =
        charP '(' *> wsP *> formula <* wsP <* charP ')'
            <|> TrueF <$ stringP "true"
            <|> FalseF <$ stringP "false"
            <|> AtomF . Prop <$> notNullP (spanP isAlphaNum)

atoms :: (Ord a) => Formula a -> [a]
atoms = Set.toList . foldr Set.insert Set.empty

eval :: Formula a -> (a -> Bool) -> Bool
eval FalseF _ = False
eval TrueF _ = True
eval (AtomF a) v = v a
eval (NotF a) v = not (eval a v)
eval (AndF a b) v = eval a v && eval b v
eval (OrF a b) v = eval a v || eval b v
eval (ImpF a b) v = not (eval a v) || eval b v
eval (IffF a b) v = eval a v == eval b v
eval _ _ = undefined

onAllValuations :: (Ord a) => Formula a -> ((a -> Bool) -> b) -> (b -> b -> b) -> b
onAllValuations fm valuate combine = f (const False) ats
  where f v [] = valuate v
        f v (q:qs) = f (v' False) qs `combine` f (v' True) qs
          where v' t p = if p == q then t else v p
        ats = atoms fm

truthTable :: (Ord a) => Formula a -> [[Bool]]
truthTable fm = onAllValuations fm valuationToList (++)
  where valuationToList v = [map v ats ++ [eval fm v]]
        ats = atoms fm

tautology :: (Ord a) => Formula a -> Bool
tautology fm = onAllValuations fm (eval fm) (&&)

unsatisfiable :: (Ord a) => Formula a -> Bool
unsatisfiable = tautology . NotF

satisfiable :: (Ord a) => Formula a -> Bool
satisfiable = not . unsatisfiable

printTruthTable :: String -> IO ()
printTruthTable s = mapM_ print $ truthTable (parse formula s)

dual :: (Show a) => Formula a -> Formula a
dual FalseF = TrueF
dual TrueF = FalseF
dual fm@(AtomF _) = fm
dual (NotF a) = NotF (dual a)
dual (AndF a b) = OrF (dual a) (dual b)
dual (OrF a b) = AndF (dual a) (dual b)
dual a = error ("dual of " ++ show a ++ " is undefined")

psimplify1 :: Formula a -> Formula a
psimplify1 a@(AndF p1 p2) = case (p1, p2) of
  (TrueF, p) -> p
  (p, TrueF) -> p
  (FalseF, _) -> FalseF
  (_, FalseF) -> FalseF
  (_, _) -> a
psimplify1 a@(OrF p1 p2) = case (p1, p2) of
  (TrueF, _) -> TrueF
  (_, TrueF) -> TrueF
  (FalseF, p) -> p
  (p, FalseF) -> p
  (_, _) -> a
psimplify1 a@(NotF p) = case p of
  NotF p' -> p'
  TrueF -> FalseF
  FalseF -> TrueF
  _p -> a
psimplify1 a@(ImpF p1 p2) = case (p1, p2) of
  (FalseF, _) -> TrueF
  (_, TrueF) -> TrueF
  (TrueF, p) -> p
  (p, FalseF) -> NotF p
  (_, _) -> a
psimplify1 a@(IffF p1 p2) = case (p1, p2) of
  (TrueF, p) -> p
  (p, TrueF) -> p
  (FalseF, p) -> NotF p
  (p, FalseF) -> NotF p
  (_, _) -> a
psimplify1 fm = fm

psimplify :: Formula a -> Formula a
psimplify (NotF a) = psimplify1 $ NotF (psimplify a)
psimplify (AndF a b) = psimplify1 $ AndF (psimplify a) (psimplify b)
psimplify (OrF a b) = psimplify1 $ OrF (psimplify a) (psimplify b)
psimplify (ImpF a b) = psimplify1 $ ImpF (psimplify a) (psimplify b)
psimplify (IffF a b) = psimplify1 $ IffF (psimplify a) (psimplify b)
psimplify fm = fm


negative :: Formula a -> Bool
negative (NotF _) = True
negative _ = False

positive :: Formula a -> Bool
positive = not . negative

negateFormula :: Formula a -> Formula a
negateFormula (NotF a) = a
negateFormula a = NotF a

nnf :: Formula a -> Formula a
nnf = f . psimplify
  where f (AndF a b) = AndF (f a) (f b)
        f (OrF a b) = OrF (f a) (f b)
        f (ImpF a b) = OrF (f (NotF a)) (f b)
        f (IffF a b) = OrF (AndF (f a) (f b)) (AndF (f (NotF a)) (f (NotF b)))
        f (NotF (NotF a)) = f a
        f (NotF (AndF a b)) = OrF (f (NotF a)) (f (NotF b))
        f (NotF (OrF a b)) = AndF (f (NotF a)) (f (NotF b))
        f (NotF (ImpF a b)) = AndF (f  a) (f (NotF b))
        f (NotF (IffF a b)) = OrF (AndF (f a) (f (NotF b))) (AndF (f (NotF a)) (f b))
        f fm = fm

nenf :: Formula a -> Formula a
nenf = f . psimplify
  where f (AndF a b) = AndF (f a) (f b)
        f (OrF a b) = OrF (f a) (f b)
        f (ImpF a b) = OrF (f (NotF a)) (f b)
        f (IffF a b) = IffF (f a) (f b)
        f (NotF (NotF a)) = f a
        f (NotF (AndF a b)) = OrF (f (NotF a)) (f (NotF b))
        f (NotF (OrF a b)) = AndF (f (NotF a)) (f (NotF b))
        f (NotF (ImpF a b)) = AndF (f  a) (f (NotF b))
        f (NotF (IffF a b)) = IffF (f a) (f (NotF b))
        f fm = fm

listConj :: (Foldable t) => t (Formula a) -> Formula a
listConj xs | null xs = TrueF
listConj xs = foldr1 AndF xs

listDisj :: (Foldable t) => t (Formula a) -> Formula a
listDisj xs | null xs = FalseF
listDisj xs = foldr1 OrF xs


dnf :: (Ord a) => Formula a -> Formula a
dnf fm = listDisj literals
  where literals = map (mkLiterals ats) valuations
        mkLiterals fms v = listConj $ map (\at -> if eval at v then at else NotF at) fms
        ats = map AtomF $ atoms fm
        valuations = onAllValuations fm (\v -> ([v | eval fm v])) (++)

rawdnf :: Formula a -> Formula a
rawdnf = f
  where f (AndF p q) = distrib (AndF (f p) (f q))
        f (OrF p q) = OrF (f p) (f q)
        f fm = fm
        distrib (AndF p (OrF q r)) = OrF (AndF p q) (AndF p r)
        distrib (AndF (OrF p q) r) = OrF (AndF p r) (AndF q r)
        distrib fm = fm

purednf :: (Ord a) => Formula a -> Set.Set (Set.Set (Formula a))
purednf = f
  where f (AndF p q) = distrib (f p) (f q)
        f (OrF p q) = Set.union (f p) (f q)
        f fm =  Set.singleton (Set.singleton fm)
        distrib s1 s2 = Set.map (uncurry Set.union) $ Set.cartesianProduct s1 s2
trivial :: (Eq a, Ord a) => Set.Set (Formula a) -> Bool
trivial fms =
  let (pos, neg) = Set.partition positive fms
   in pos `Set.intersection` Set.map negateFormula neg /= Set.empty


simpdnf :: (Ord a, Eq a) => Formula a -> Set.Set (Set.Set (Formula a))
simpdnf FalseF = Set.empty
simpdnf TrueF = Set.singleton Set.empty
simpdnf fm =
  let djs = Set.filter (not . trivial) (purednf (nnf fm))
   in Set.filter (\d -> not $ any (`Set.isProperSubsetOf` d) djs) djs

setDnf :: (Eq a, Ord a) => Formula a -> Formula a
setDnf fm = listDisj (Set.map listConj (simpdnf fm))


purecnf :: (Ord a) => Formula a -> Set.Set (Set.Set (Formula a))
purecnf fm = Set.map (Set.map negateFormula) (purednf (nnf (NotF fm)))

simpcnf :: (Ord a, Eq a) => Formula a -> Set.Set (Set.Set (Formula a))
simpcnf TrueF = Set.empty
simpcnf FalseF = Set.singleton Set.empty
simpcnf fm =
  let djs = Set.filter (not . trivial) (purecnf (nnf fm))
   in Set.filter (\d -> not $ any (`Set.isProperSubsetOf` d) djs) djs

cnf :: (Eq a, Ord a) => Formula a -> Formula a
cnf fm = listConj (Set.map listDisj (simpcnf fm))
main :: IO ()
main = do
    content <- getContents
    mapM_ (print . parse formula) $ lines content
