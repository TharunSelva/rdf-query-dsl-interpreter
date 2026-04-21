module RQL.Eval.Builtins
  ( compareTerms
  , evalPred
  ) where

import qualified Data.Map as Map
import RQL.Common.Types
import RQL.Syntax.AST

-- Predicate evaluation

lookupBinding :: PatternTerm -> Binding -> Maybe Term
lookupBinding (PConst t) _ = Just t
lookupBinding (PVar v) b =
  Map.lookup v b

compareTerms :: Term -> Term -> Maybe Ordering
compareTerms (TLit (LInt a)) (TLit (LInt b)) = Just (compare a b)
compareTerms (TLit (LString a)) (TLit (LString b)) = Just (compare a b)
compareTerms (TIri a) (TIri b) = Just (compare a b)
compareTerms _ _ = Nothing

orderedComparison :: (Ordering -> Bool) -> PatternTerm -> PatternTerm -> Binding -> Bool
orderedComparison keep left right binding =
  case (lookupBinding left binding, lookupBinding right binding) of
    (Just leftTerm, Just rightTerm) ->
      case compareTerms leftTerm rightTerm of
        Just ordering -> keep ordering
        Nothing -> False
    _ ->
      False

evalPred :: Predicate -> Binding -> Bool
evalPred (PEq l r) b =
  case (lookupBinding l b, lookupBinding r b) of
    (Just leftTerm, Just rightTerm) -> leftTerm == rightTerm
    _ -> False
evalPred (PNe l r) b =
  case (lookupBinding l b, lookupBinding r b) of
    (Just leftTerm, Just rightTerm) -> leftTerm /= rightTerm
    _ -> False
evalPred (PLt l r) b = orderedComparison (== LT) l r b
evalPred (PGt l r) b = orderedComparison (== GT) l r b
evalPred (PGe l r) b = orderedComparison (`elem` [GT, EQ]) l r b
evalPred (PLe l r) b = orderedComparison (`elem` [LT, EQ]) l r b
evalPred (PNot p) b = not (evalPred p b)
evalPred (PAnd p q) b = evalPred p b && evalPred q b
evalPred (POr p q) b = evalPred p b || evalPred q b
