module RQL.Turtle.Lower (lower) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.List       (isPrefixOf)
import           RQL.Common.Types
import           RQL.Turtle.AST

-- ---------------------------------------------------------------------------
-- Environment accumulated while processing @base / @prefix directives
-- ---------------------------------------------------------------------------

data Env = Env
  { envBase     :: String
  , envPrefixes :: Map.Map String String
  }

emptyEnv :: Env
emptyEnv = Env { envBase = "", envPrefixes = Map.empty }

-- ---------------------------------------------------------------------------
-- Public entry point
-- ---------------------------------------------------------------------------

-- | Resolve all base/prefix declarations and flatten predicate/object lists
-- into a 'Graph'.  Uses 'error' on undeclared prefixes (caught by Loader).
lower :: TurtleDoc -> Graph
lower (TurtleDoc stmts) = Set.fromList triples
  where
    (_, triples) = foldl step (emptyEnv, []) stmts

    step (env, acc) (TBase uri)       = (env { envBase = uri }, acc)
    step (env, acc) (TPrefix ns uri)  =
      (env { envPrefixes = Map.insert ns uri (envPrefixes env) }, acc)
    step (env, acc) (TTriples s pols) =
      (env, acc ++ expandTriples env s pols)

-- ---------------------------------------------------------------------------
-- Expansion helpers
-- ---------------------------------------------------------------------------

expandTriples :: Env -> TSubject -> [TPredObjList] -> [Triple]
expandTriples env subj pols =
  let s = resolveSubject env subj
  in  [ Triple s (resolvePred env p) (resolveObj env o)
      | (p, objs) <- pols
      , o         <- objs
      ]

resolveSubject :: Env -> TSubject -> Term
resolveSubject env (TSIri iri)       = TIri (resolveIri env iri)
resolveSubject env (TSPName pre loc) = TIri (resolvePName env pre loc)

resolvePred :: Env -> TPred -> Term
resolvePred env (TPIri iri)       = TIri (resolveIri env iri)
resolvePred env (TPPName pre loc) = TIri (resolvePName env pre loc)
resolvePred _   TPA               = TIri rdfType

resolveObj :: Env -> TObj -> Term
resolveObj env (TOIri iri)       = TIri (resolveIri env iri)
resolveObj env (TOPName pre loc) = TIri (resolvePName env pre loc)
resolveObj _   (TOLit lit)       = TLit lit

-- | Resolve an IRI: if already absolute leave it alone, otherwise prepend base.
resolveIri :: Env -> String -> String
resolveIri env iri
  | isAbsolute iri = iri
  | otherwise      = envBase env ++ iri

isAbsolute :: String -> Bool
isAbsolute iri =
  any (`isPrefixOf` iri) ["http://", "https://", "ftp://", "urn:"]

-- | Expand a prefixed name using the prefix map.
resolvePName :: Env -> String -> String -> String
resolvePName env pre loc =
  case Map.lookup pre (envPrefixes env) of
    Just base -> base ++ loc
    Nothing   ->
      error ("RQL.Turtle.Lower: undeclared prefix '" ++ pre ++ "'")

-- | The IRI for @rdf:type@, expanded form of the @a@ keyword.
rdfType :: String
rdfType = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
