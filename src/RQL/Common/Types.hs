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

data Triple = Triple 
  { subject :: Term
  , predicate :: Term
  , object :: Term
  } deriving (Eq, Ord, Show)

type Graph = Set Triple

data PatternTerm
  = PVar VarName
  | PConst Term
  deriving (Eq, Ord, Show)

data TriplePattern = TriplePattern PatternTerm PatternTerm PatternTerm
  deriving (Eq, Ord, Show)

type Binding = Map VarName Term

type Name = String

type GraphEnv = Map Name Graph

type BindEnv = Map Name [Binding]

data Value
 = VGraph Graph
 | VBindings [Binding]
 deriving (Eq, Ord, Show)
