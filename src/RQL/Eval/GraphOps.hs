module RQL.Eval.GraphOps where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (sortBy, groupBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

import RQL.Common.Types

-- Graph operations

graphUnion :: Graph -> Graph -> Graph
graphUnion = Set.union

graphIntersection :: Graph -> Graph -> Graph
graphIntersection = Set.intersection

graphDifference :: Graph -> Graph -> Graph
graphDifference = Set.difference

-- Pattern matching

bindTerm :: PatternTerm -> Term -> Binding -> Maybe Binding
bindTerm (PConst c) t b
  | c == t    = Just b
  | otherwise = Nothing
bindTerm (PVar v) t b =
  case Map.lookup v b of
    Nothing -> Just (Map.insert v t b)
    Just t' -> if t' == t then Just b else Nothing

matchTriple :: TriplePattern -> Triple -> Maybe Binding
matchTriple (TriplePattern ps pp po) (Triple s p o) =
  bindTerm ps s Map.empty >>= bindTerm pp p >>= bindTerm po o

matchGraph :: TriplePattern -> Graph -> [Binding]
matchGraph pat g = mapMaybe (matchTriple pat) (Set.toList g)

-- Constructing output triples

instantiateTerm :: PatternTerm -> Binding -> Term
instantiateTerm (PConst t) _ = t
instantiateTerm (PVar v) b =
  case Map.lookup v b of
    Just t -> t
    Nothing -> error ("RQL construct: unbound variable: " ++ v)

instantiateTriple :: TriplePattern -> Binding -> Triple
instantiateTriple (TriplePattern s p o) b =
  Triple (instantiateTerm s b) (instantiateTerm p b) (instantiateTerm o b)

-- GroupMax and GroupMin helpers

intValue :: Term -> Int
intValue (TLit (LInt n)) = n
intValue t = error ("RQL groupmax: expected integer, got: " ++ show t)

groupByPredicate :: (Int -> Int -> Bool) -> Iri -> Graph -> Graph
groupByPredicate cmp iri triples =
  let triplesList = Set.toList triples
      grouped = groupBy sameSubj $ sortBy (comparing subject) triplesList
      keepBest grp =
        let candidates = filter (\t -> predicate t == TIri iri) grp
        in case candidates of
             [] -> []
             _  -> [foldr1 (\a b -> if cmp (intValue (object a)) (intValue (object b)) then a else b) candidates]
  in Set.fromList (concatMap keepBest grouped)
  where sameSubj a b = subject a == subject b

groupMax :: VarName -> Iri -> Graph -> Graph
groupMax _ = groupByPredicate (>=)

groupMin :: VarName -> Iri -> Graph -> Graph
groupMin _ = groupByPredicate (<=)

-- Chain: compose two graphs along a shared intermediate node.
-- For each path x -p1-> z in g1 and z -p2-> y in g2, emit x -p2-> y.
-- This collapses x -> z -> y into x -> y, removing the middle node.

chainGraphs :: Graph -> Graph -> Graph
chainGraphs g1 g2 =
  let pairs = [ Triple (subject t1) (predicate t2) (object t2)
              | t1 <- Set.toList g1
              , t2 <- Set.toList g2
              , object t1 == subject t2
              ]
  in Set.fromList pairs

-- Type inference for Task 5

rdfType :: Term
rdfType = TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

rdfsSubClassOf :: Term
rdfsSubClassOf = TIri "http://www.w3.org/2000/01/rdf-schema#subClassOf"

inferTypes :: Graph -> Graph -> Graph
inferTypes typeGraph subclassGraph =
  Set.fromList
    [ Triple subj rdfType superClass
    | Triple subj predType cls <- Set.toList typeGraph
    , predType == rdfType
    , isIriTerm subj
    , isIriTerm cls
    , Triple subClass predSub superClass <- Set.toList subclassGraph
    , predSub == rdfsSubClassOf
    , cls == subClass
    , isIriTerm subClass
    , isIriTerm superClass
    ]
  where
    isIriTerm :: Term -> Bool
    isIriTerm (TIri _) = True
    isIriTerm _ = False