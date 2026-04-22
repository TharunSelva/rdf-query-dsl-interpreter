module RQL.Eval.GraphOps
  ( chainGraphs
  , graphDifference
  , graphIntersection
  , graphUnion
  , groupMax
  , groupMin
  , inferTypes
  , instantiateGraph
  , instantiateTriple
  , joinBindings
  , matchGraph
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (foldM)
import Data.List (groupBy, nub, sortBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)

import RQL.Common.Types
import RQL.Eval.Builtins (compareTerms)

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

compatibleBindings :: Binding -> Binding -> Bool
compatibleBindings left right =
  let sharedKeys = Map.keysSet left `Set.intersection` Map.keysSet right
  in all sameTerm (Set.toList sharedKeys)
  where
    sameTerm key = Map.lookup key left == Map.lookup key right

joinBindings :: [Binding] -> [Binding] -> [Binding]
joinBindings left right =
  nub
    [ Map.union leftBinding rightBinding
    | leftBinding <- left
    , rightBinding <- right
    , compatibleBindings leftBinding rightBinding
    ]

matchTriple :: TriplePattern -> Triple -> Maybe Binding
matchTriple (TriplePattern ps pp po) (Triple s p o) =
  bindTerm ps s Map.empty >>= bindTerm pp p >>= bindTerm po o

matchGraph :: TriplePattern -> Graph -> [Binding]
matchGraph pat g = mapMaybe (matchTriple pat) (Set.toList g)

-- Constructing triples

instantiateTerm :: PatternTerm -> Binding -> Maybe Term
instantiateTerm (PConst t) _ = Just t
instantiateTerm (PVar v) b = Map.lookup v b

validateConstructTemplate :: TriplePattern -> Either String ()
validateConstructTemplate (TriplePattern s p _) =
  case (s, p) of
    (PConst subjectTerm@(TLit _), _) ->
      Left ("RQL construct: subject must be an IRI, got: " ++ show subjectTerm)
    (_, PConst predicateTerm@(TLit _)) ->
      Left ("RQL construct: predicate must be an IRI, got: " ++ show predicateTerm)
    _ ->
      Right ()

instantiateTriple :: TriplePattern -> Binding -> Either String (Maybe Triple)
instantiateTriple pattern_@(TriplePattern s p o) b = do
  validateConstructTemplate pattern_
  case (instantiateTerm s b, instantiateTerm p b, instantiateTerm o b) of
    (Just subjectTerm, Just predicateTerm, Just objectTerm) ->
      case (subjectTerm, predicateTerm) of
        (TLit _, _) -> Right Nothing
        (_, TLit _) -> Right Nothing
        _ -> Right (Just (Triple subjectTerm predicateTerm objectTerm))
    _ ->
      Right Nothing

instantiateGraph :: [TriplePattern] -> [Binding] -> Either String Graph
instantiateGraph patterns bindings = do
  triples <- mapM instantiateBinding bindings
  pure (Set.fromList (catMaybes (concat triples)))
  where
    instantiateBinding binding = mapM (`instantiateTriple` binding) patterns

-- GroupMax and GroupMin helpers

groupKey :: VarName -> Triple -> Either String Term
groupKey "s" = Right . subject
groupKey "p" = Right . predicate
groupKey "o" = Right . object
groupKey varName = const (Left ("RQL grouping: grouping variable must be ?s, ?p, or ?o, got: ?" ++ varName))

chooseBest :: (Ordering -> Bool) -> Triple -> Triple -> Either String Triple
chooseBest keep current candidate =
  case compareTerms (object candidate) (object current) of
    Just ordering ->
      Right (if keep ordering then candidate else current)
    Nothing ->
      Left
        ( "RQL grouping: cannot compare objects "
            ++ show (object current)
            ++ " and "
            ++ show (object candidate)
        )

groupByPredicate :: (Ordering -> Bool) -> VarName -> Iri -> Graph -> Either String Graph
groupByPredicate keep groupVar iri triples = do
  keyedTriples <- mapM addKey (Set.toList triples)
  groupedTriples <- mapM keepBestGroup (groupBy sameKey (sortBy (comparing fst) keyedTriples))
  pure (Set.fromList (concat groupedTriples))
  where
    addKey triple = do
      key <- groupKey groupVar triple
      pure (key, triple)

    sameKey (leftKey, _) (rightKey, _) = leftKey == rightKey

    keepBestGroup keyedGroup =
      let candidates = filter hasAggregatePredicate (map snd keyedGroup)
      in case candidates of
           [] -> Right []
           firstTriple : remaining ->
             do
               bestTriple <- foldM (chooseBest keep) firstTriple remaining
               pure [bestTriple]

    hasAggregatePredicate triple = predicate triple == TIri iri

groupMax :: VarName -> Term -> Graph -> Either String Graph
groupMax "s" aggregatePredicate triples = do
  aggregateIri <- requireAggregateIri aggregatePredicate
  let matchingTriples =
        filter ((== TIri aggregateIri) . predicate) (Set.toList triples)
      groupedBySubject =
        groupBy sameSubject (sortBy (comparing subject) matchingTriples)
      sameSubject left right = subject left == subject right
  groupedTriples <- mapM chooseMaxIntegerGroup groupedBySubject
  pure (Set.fromList groupedTriples)
  where
    chooseMaxIntegerGroup [] =
      Left "RQL groupmax: internal error: encountered an empty subject group"
    chooseMaxIntegerGroup (firstTriple : remainingTriples) =
      foldM chooseMaxIntegerTriple firstTriple remainingTriples

groupMax groupVar _ _ =
  Left ("RQL groupmax: Task 4 only supports grouping by ?s, got: ?" ++ groupVar)

requireAggregateIri :: Term -> Either String Iri
requireAggregateIri (TIri iri) = Right iri
requireAggregateIri predicateTerm =
  Left ("RQL groupmax: predicate must be an IRI, got: " ++ show predicateTerm)

chooseMaxIntegerTriple :: Triple -> Triple -> Either String Triple
chooseMaxIntegerTriple current candidate = do
  currentValue <- integerObjectValue current
  candidateValue <- integerObjectValue candidate
  pure (if candidateValue > currentValue then candidate else current)

integerObjectValue :: Triple -> Either String Int
integerObjectValue triple =
  case object triple of
    TLit (LInt value) ->
      Right value
    objectTerm ->
      Left
        ( "RQL groupmax: expected an integer object for predicate "
            ++ show (predicate triple)
            ++ ", got: "
            ++ show objectTerm
        )

groupMin :: VarName -> Iri -> Graph -> Either String Graph
groupMin = groupByPredicate (`elem` [LT, EQ])

-- Chain: compose two graphs along a shared intermediate node.

chainGraphs :: Graph -> Graph -> Graph
chainGraphs g1 g2 =
  let pairs = [ Triple (subject t1) (predicate t2) (object t2)
              | t1 <- Set.toList g1
              , t2 <- Set.toList g2
              , object t1 == subject t2
              ]
  in Set.fromList pairs

-- Type inference

rdfType :: Term
rdfType = TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"

inferTypes :: Graph -> Graph -> Graph
inferTypes schema instances =
  let schemaSubjects = Set.map subject schema
      relevant = Set.filter (\t -> subject t `elem` schemaSubjects) instances
      mkTypeTriple t = Triple (subject t) rdfType (predicate t)
  in Set.fromList (nub (map mkTypeTriple (Set.toList relevant)))
