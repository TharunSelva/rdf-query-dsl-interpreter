module RQL.Common.Types where

import Data.Map (Map)
import Data.Set (Set)

type GraphName = String
type VarName = String
type Iri = String

data Literal
  = LString String
  | LInt Int
  deriving (Eq, Ord, Show)

data Term
  = TIri Iri
  | TLit Literal
  deriving (Eq, Ord, Show)

data Triple = Triple Term Term Term
  deriving (Eq, Ord, Show)

type Graph = Set Triple

data PatternTerm
  = PVar VarName
  | PConst Term
  deriving (Eq, Ord, Show)

data TriplePattern = TriplePattern PatternTerm PatternTerm PatternTerm
  deriving (Eq, Ord, Show)

type Binding = Map VarName Term
