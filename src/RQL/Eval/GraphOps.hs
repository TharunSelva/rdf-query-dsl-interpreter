module RQL.Eval.GraphOps where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (nub, sortBy, groupBy)
import Data.Ord (comparing, Down(..))
import Data.Maybe (mapMaybe)

import RQL.Common.Types

-- Graph set operations

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

-- GroupMax helper

intValue :: Term -> Int
intValue (TLit (LInt n)) = n
intValue t = error ("RQL groupmax: expected integer, got: " ++ show t)

groupMax :: VarName -> Iri -> Graph -> Graph
groupMax groupVar maxIri triples =
  let triplesList = Set.toList triples
      grouped = groupBy sameSubj $ sortBy (comparing subject) triplesList
      keepBest grp =
        let candidates = filter (\t -> predicate t == TIri maxIri) grp
        in case candidates of
             [] -> []
             _  -> [maximumByObj candidates]
  in Set.fromList (concatMap keepBest grouped)
  where
    sameSubj a b = subject a == subject b
    maximumByObj = foldr1 (\a b -> if intValue (object a) >= intValue (object b) then a else b)

-- Type inference

rdfType :: Term
rdfType = TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

inferTypes :: Graph -> Graph -> Graph
inferTypes schema instances =
  let schemaSubjects = Set.map subject schema
      relevant = Set.filter (\t -> subject t `elem` schemaSubjects) instances
      mkTypeTriple t = Triple (subject t) rdfType (predicate t)
  in Set.fromList (nub (map mkTypeTriple (Set.toList relevant)))
