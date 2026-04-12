module RQL.Turtle.Lower (lower) where

import Control.Exception  (throw)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           Data.List       (isPrefixOf)
import           RQL.Common.Errors (TurtleException (TurtlePrefixException))
import           RQL.Common.Types
import           RQL.Turtle.AST

data Env = Env
  { envBase     :: String
  , envPrefixes :: Map.Map String String
  }

emptyEnv :: Env
emptyEnv = Env { envBase = "", envPrefixes = Map.empty }

lower :: TurtleDoc -> Graph
lower (TurtleDoc stmts) = Set.fromList (go emptyEnv stmts)
  where
    go _ [] = []
    go env (stmt : rest) =
      case stmt of
        TBase uri ->
          go env { envBase = uri } rest
        TPrefix prefix uri ->
          go env { envPrefixes = Map.insert prefix uri (envPrefixes env) } rest
        TTriples subj predicateObjects ->
          expandTriples env subj predicateObjects ++ go env rest

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

resolveIri :: Env -> String -> String
resolveIri env iri
  | isAbsolute iri = iri
  | otherwise      = envBase env ++ iri

isAbsolute :: String -> Bool
isAbsolute iri =
  any (`isPrefixOf` iri) ["http://", "https://", "ftp://", "urn:"]

resolvePName :: Env -> String -> String -> String
resolvePName env pre loc =
  case Map.lookup pre (envPrefixes env) of
    Just base -> base ++ loc
    Nothing   -> throw (TurtlePrefixException pre)

rdfType :: String
rdfType = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
