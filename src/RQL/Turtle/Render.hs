module RQL.Turtle.Render (renderGraph) where

import qualified Data.Set as Set
import RQL.Common.Types

-- | Render a graph as canonical N-Triples: one triple per line, all URIs
-- fully expanded in angle brackets, sorted lexicographically, no duplicates.
renderGraph :: Graph -> String
renderGraph g = unlines (map renderTriple (Set.toAscList g))

renderTriple :: Triple -> String
renderTriple (Triple s p o) =
  renderTerm s ++ " " ++ renderTerm p ++ " " ++ renderTerm o ++ " ."

renderTerm :: Term -> String
renderTerm (TIri iri)           = "<" ++ iri ++ ">"
renderTerm (TLit (LString str)) = "\"" ++ str ++ "\""
renderTerm (TLit (LInt n))      = show n
