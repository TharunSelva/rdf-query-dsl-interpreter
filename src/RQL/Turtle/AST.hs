module RQL.Turtle.AST
  ( TurtleDoc (..)
  , TStmt (..)
  , TSubject (..)
  , TPred (..)
  , TObj (..)
  , TPredObjList
  ) where

import RQL.Common.Types (Literal)

-- | A complete Turtle document: an ordered list of statements.
newtype TurtleDoc = TurtleDoc [TStmt]
  deriving (Eq, Show)

-- | Top-level statements in a Turtle document.
data TStmt
  = TBase   String              -- ^ @base \<iri\> .
  | TPrefix String String       -- ^ @prefix ns: \<iri\> .
  | TTriples TSubject [TPredObjList]  -- ^ subject predicate-object-list+
  deriving (Eq, Show)

-- | Subject node: full IRI or prefixed name.
data TSubject
  = TSIri   String         -- ^ \<iri\>
  | TSPName String String  -- ^ prefix local
  deriving (Eq, Show)

-- | Predicate node: full IRI, prefixed name, or the @a@ keyword.
data TPred
  = TPIri   String
  | TPPName String String
  | TPA                    -- ^ @a@  ≡  rdf:type
  deriving (Eq, Show)

-- | Object node: full IRI, prefixed name, or literal.
data TObj
  = TOIri   String
  | TOPName String String
  | TOLit   Literal
  deriving (Eq, Show)

-- | One predicate paired with its list of objects.
type TPredObjList = (TPred, [TObj])
