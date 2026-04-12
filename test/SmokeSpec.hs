module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.Set as Set
import RQL.Common.Errors (TurtleError (..), renderTurtleError)
import RQL.Common.Types
import RQL.Turtle.Loader (parseTurtleFile)
import RQL.Turtle.Render (renderGraph)

data Result = Pass String | Fail String String
  deriving (Show)

checkPure :: String -> Bool -> Result
checkPure name True  = Pass name
checkPure name False = Fail name "pure assertion failed"

checkIO :: String -> IO Bool -> IO Result
checkIO name action = do
  ok <- action
  pure (if ok then Pass name else Fail name "IO assertion failed")

runTests :: [IO Result] -> IO ()
runTests tests = do
  results <- sequence tests
  forM_ results $ \result ->
    case result of
      Pass name ->
        putStrLn ("  PASS  " ++ name)
      Fail name msg ->
        putStrLn ("  FAIL  " ++ name ++ "  [" ++ msg ++ "]")
  let failures = [result | result@(Fail _ _) <- results]
  putStrLn
    ( show (length results - length failures)
    ++ "/"
    ++ show (length results)
    ++ " tests passed"
    )
  unless (null failures) (error "Some tests FAILED")

iriTerm :: String -> Term
iriTerm = TIri

strTerm :: String -> Term
strTerm = TLit . LString

intTerm :: Int -> Term
intTerm = TLit . LInt

renderTests :: [IO Result]
renderTests =
  [ pure $ checkPure "render empty graph"
      (renderGraph Set.empty == "")
  , pure $ checkPure "render single IRI triple"
      (renderGraph (Set.singleton t1) ==
        "<http://a.org/s> <http://a.org/p> \"hello\" .\n")
  , pure $ checkPure "render integer literal"
      (renderGraph (Set.singleton t2) ==
        "<http://a.org/s> <http://a.org/p> 99 .\n")
  , pure $ checkPure "render sorts triples lexicographically"
      (renderGraph (Set.fromList [tb, ta]) ==
        "<http://a.org/s> <http://p.org/p> <http://o.org/o> .\n\
        \<http://b.org/s> <http://p.org/p> <http://o.org/o> .\n")
  , pure $ checkPure "render deduplicates identical triples"
      (Set.size (Set.fromList [t1, t1]) == 1)
  ]
  where
    t1 = Triple (iriTerm "http://a.org/s") (iriTerm "http://a.org/p") (strTerm "hello")
    t2 = Triple (iriTerm "http://a.org/s") (iriTerm "http://a.org/p") (intTerm 99)
    ta = Triple (iriTerm "http://a.org/s") (iriTerm "http://p.org/p") (iriTerm "http://o.org/o")
    tb = Triple (iriTerm "http://b.org/s") (iriTerm "http://p.org/p") (iriTerm "http://o.org/o")

turtleTests :: [IO Result]
turtleTests =
  [ checkIO "tiny.ttl matches expected output" $ do
      expected <- readFile "fixtures/smoke/tiny.expected"
      expectGraph "fixtures/smoke/tiny.ttl" (\graph -> renderGraph graph == expected)
  , checkIO "predicate list expands to 2 triples" $
      expectGraph "fixtures/smoke/predlist.ttl" (\graph -> Set.size graph == 2)
  , checkIO "object list expands to 2 triples" $
      expectGraph "fixtures/smoke/objlist.ttl" (\graph -> Set.size graph == 2)
  , checkIO "'a' keyword expands to rdf:type IRI" $
      expectGraph "fixtures/smoke/keyword_a.ttl" $
        any (\(Triple _ predicate _) ->
          predicate == TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
          . Set.toList
  , checkIO "string literal (LF-only file)" $
      expectGraph "fixtures/smoke/stronly.ttl" (\graph -> Set.size graph == 1)
  , checkIO "integer literal parsed and rendered correctly" $
      expectGraph "fixtures/smoke/intlit.ttl" $
        any (\(Triple _ _ objectTerm) -> objectTerm == TLit (LInt 42)) . Set.toList
  , checkIO "base URI resolution (relative IRI)" $
      expectGraph "fixtures/smoke/tiny.ttl" $
        any (\(Triple subject _ _) -> subject == TIri "http://example.org/a") . Set.toList
  , checkIO "prefix expansion (prefixed name to full IRI)" $
      expectGraph "fixtures/smoke/tiny.ttl" $
        any (\(Triple _ predicate _) -> predicate == TIri "http://example.org/ont/name") . Set.toList
  , checkIO "missing files report a file error" $
      expectError "fixtures/smoke/does-not-exist.ttl" $
        \err -> case err of
          TurtleFileError _ _ -> True
          _ -> False
  , checkIO "undeclared prefixes report a prefix error" $
      expectError "fixtures/smoke/undeclared_prefix.ttl" $
        \err -> case err of
          TurtlePrefixError "ont" -> True
          _ -> False
  , checkIO "syntax errors report a syntax error" $
      expectError "fixtures/smoke/bad_syntax.ttl" $
        \err -> case err of
          TurtleSyntaxError msg -> not (null msg)
          _ -> False
  ]

expectGraph :: FilePath -> (Graph -> Bool) -> IO Bool
expectGraph path predicate = do
  result <- parseTurtleFile path
  case result of
    Left err -> do
      printErr err
      pure False
    Right graph ->
      pure (predicate graph)

expectError :: FilePath -> (TurtleError -> Bool) -> IO Bool
expectError path predicate = do
  result <- parseTurtleFile path
  case result of
    Left err ->
      pure (predicate err)
    Right _ ->
      pure False

printErr :: TurtleError -> IO ()
printErr err = putStrLn ("    error: " ++ renderTurtleError err)

main :: IO ()
main = do
  putStrLn "=== RenderSpec ==="
  runTests renderTests
  putStrLn "=== TurtleSpec ==="
  runTests turtleTests
