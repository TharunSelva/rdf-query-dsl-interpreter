module RQL.Eval.Builtins where

import qualified Data.Map as Map
import RQL.Common.Types
import RQL.Syntax.AST

-- Predicate evaluation

lookupBinding :: PatternTerm -> Binding -> Term
lookupBinding (PConst t) _ = t
lookupBinding (PVar v) b =
  case Map.lookup v b of
    Just t -> t
    Nothing -> error ("RQL filter: unbound variable: " ++ v)

compareTerms :: Term -> Term -> Maybe Ordering
compareTerms (TLit (LInt a)) (TLit (LInt b)) = Just (compare a b)
compareTerms (TLit (LString a)) (TLit (LString b)) = Just (compare a b)
compareTerms (TIri a) (TIri b) = Just (compare a b)
compareTerms _ _ = Nothing

evalPred :: Predicate -> Binding -> Bool
evalPred (PEq l r) b = lookupBinding l b == lookupBinding r b
evalPred (PGe l r) b =
  case compareTerms (lookupBinding l b) (lookupBinding r b) of
    Just o -> o `elem` [GT, EQ]
    Nothing -> False
evalPred (PAnd p q) b = evalPred p b && evalPred q b
evalPred (POr p q) b = evalPred p b || evalPred q b
