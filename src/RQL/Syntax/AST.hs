module RQL.Syntax.AST where

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
  | EGroupMax Expr VarName Iri
  | ETypeInfer Expr Expr
  deriving (Eq, Show)

data GraphPattern = GraphPattern
  { gpGraph :: String
  , gpPattern :: TriplePattern
  }
  deriving (Eq, Show)

data Predicate
  = PEq PatternTerm PatternTerm
  | PGe PatternTerm PatternTerm
  | PAnd Predicate Predicate
  | POr Predicate Predicate
  deriving (Eq, Show)