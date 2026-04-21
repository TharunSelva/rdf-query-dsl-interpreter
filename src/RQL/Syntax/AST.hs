module RQL.Syntax.AST
  ( Program (..)
  , Stmt (..)
  , Expr (..)
  , GraphPattern (..)
  , Predicate (..)
  ) where

import RQL.Common.Types

data Program = Program [Stmt]
  deriving (Eq, Show)

data Stmt
  = SLet String Expr
  | SPrint Expr
  deriving (Eq, Show)

data Expr
  = EVar String
  | ELoad GraphName
  | EUnion Expr Expr
  | EIntersection Expr Expr
  | EDifference Expr Expr
  | EMatch [GraphPattern]
  | EFilter Predicate Expr
  | EConstruct [TriplePattern] Expr
  | EGroupMax Expr VarName Term
  | EGroupMin Expr VarName Iri
  | EChain Expr Expr
  | ETypeInfer Expr Expr
  deriving (Eq, Show)

data GraphPattern = GraphPattern
  { gpGraph :: String
  , gpPattern :: TriplePattern
  }
  deriving (Eq, Show)

data Predicate
  = PEq PatternTerm PatternTerm
  | PNe PatternTerm PatternTerm
  | PLt PatternTerm PatternTerm
  | PGt PatternTerm PatternTerm
  | PGe PatternTerm PatternTerm
  | PLe PatternTerm PatternTerm
  | PNot Predicate
  | PAnd Predicate Predicate
  | POr Predicate Predicate
  deriving (Eq, Show)
