module RQL.Turtle.Render (renderGraph) where

import qualified Data.List as List
import qualified Data.Set as Set
import RQL.Common.Types

renderGraph :: Graph -> String
renderGraph g =
  unlines (map renderTriple (List.sortBy compareTriple (Set.toList g)))

compareTriple :: Triple -> Triple -> Ordering
compareTriple (Triple s1 p1 o1) (Triple s2 p2 o2) =
  compare (renderTerm s1, renderTerm p1, renderTerm o1)
          (renderTerm s2, renderTerm p2, renderTerm o2)

renderTriple :: Triple -> String
renderTriple (Triple s p o) =
  renderTerm s ++ " " ++ renderTerm p ++ " " ++ renderTerm o ++ " ."

renderTerm :: Term -> String
renderTerm (TIri iri)           = "<" ++ iri ++ ">"
renderTerm (TLit (LString str)) = "\"" ++ str ++ "\""
renderTerm (TLit (LInt n))      = show n
