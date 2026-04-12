module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.Set as Set
import RQL.Common.Types
import RQL.Turtle.Loader (parseTurtleFile)
import RQL.Turtle.Render (renderGraph)

-- ---------------------------------------------------------------------------
-- Minimal test harness (no external framework needed)
-- ---------------------------------------------------------------------------

data Result = Pass String | Fail String String
  deriving (Show)

checkPure :: String -> Bool -> Result
checkPure name True  = Pass name
checkPure name False = Fail name "pure assertion failed"

checkIO :: String -> IO Bool -> IO Result
checkIO name action = do
  ok <- action
  return (if ok then Pass name else Fail name "IO assertion failed")

runTests :: [IO Result] -> IO ()
runTests tests = do
  results <- sequence tests
  forM_ results $ \r -> case r of
    Pass name     -> putStrLn ("  PASS  " ++ name)
    Fail name msg -> putStrLn ("  FAIL  " ++ name ++ "  [" ++ msg ++ "]")
  let failures = [r | r@(Fail _ _) <- results]
  putStrLn ( show (length results - length failures)
          ++ "/" ++ show (length results)
          ++ " tests passed" )
  unless (null failures) (error "Some tests FAILED")

-- ---------------------------------------------------------------------------
-- Shared fixtures
-- ---------------------------------------------------------------------------

iriTerm :: String -> Term
iriTerm = TIri

strTerm :: String -> Term
strTerm = TLit . LString

intTerm :: Int -> Term
intTerm = TLit . LInt

-- ---------------------------------------------------------------------------
-- RenderSpec
-- ---------------------------------------------------------------------------

renderTests :: [IO Result]
renderTests =
  [ return $ checkPure "render empty graph"
      (renderGraph Set.empty == "")

  , return $ checkPure "render single IRI triple"
      (renderGraph (Set.singleton t1) ==
        "<http://a.org/s> <http://a.org/p> \"hello\" .\n")

  , return $ checkPure "render integer literal"
      (renderGraph (Set.singleton t2) ==
        "<http://a.org/s> <http://a.org/p> 99 .\n")

  , return $ checkPure "render sorts triples lexicographically"
      (renderGraph (Set.fromList [tb, ta]) ==
        "<http://a.org/s> <http://p.org/p> <http://o.org/o> .\n\
        \<http://b.org/s> <http://p.org/p> <http://o.org/o> .\n")

  , return $ checkPure "render deduplicates identical triples"
      (Set.size (Set.fromList [t1, t1]) == 1)
  ]
  where
    t1 = Triple (iriTerm "http://a.org/s") (iriTerm "http://a.org/p") (strTerm "hello")
    t2 = Triple (iriTerm "http://a.org/s") (iriTerm "http://a.org/p") (intTerm 99)
    ta = Triple (iriTerm "http://a.org/s") (iriTerm "http://p.org/p") (iriTerm "http://o.org/o")
    tb = Triple (iriTerm "http://b.org/s") (iriTerm "http://p.org/p") (iriTerm "http://o.org/o")

-- ---------------------------------------------------------------------------
-- TurtleSpec  (integration: file → parseTurtleFile → renderGraph)
-- ---------------------------------------------------------------------------

turtleTests :: [IO Result]
turtleTests =
  [ checkIO "tiny.ttl matches expected output" $ do
      result   <- parseTurtleFile "fixtures/smoke/tiny.ttl"
      expected <- readFile        "fixtures/smoke/tiny.expected"
      case result of
        Left  err -> printErr err >> return False
        Right g   -> return (renderGraph g == expected)

  , checkIO "predicate list expands to 2 triples" $ do
      result <- parseTurtleFile "fixtures/smoke/predlist.ttl"
      case result of
        Left  err -> printErr err >> return False
        Right g   -> return (Set.size g == 2)

  , checkIO "object list expands to 2 triples" $ do
      result <- parseTurtleFile "fixtures/smoke/objlist.ttl"
      case result of
        Left  err -> printErr err >> return False
        Right g   -> return (Set.size g == 2)

  , checkIO "'a' keyword expands to rdf:type IRI" $ do
      result <- parseTurtleFile "fixtures/smoke/keyword_a.ttl"
      case result of
        Left  err -> printErr err >> return False
        Right g   ->
          return (any (\(Triple _ p _) ->
            p == TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
            (Set.toList g))

  , checkIO "string literal (LF-only file)" $ do
      result <- parseTurtleFile "fixtures/smoke/stronly.ttl"
      case result of
        Left  err -> printErr err >> return False
        Right g   -> return (Set.size g == 1)

  , checkIO "integer literal parsed and rendered correctly" $ do
      result <- parseTurtleFile "fixtures/smoke/intlit.ttl"
      case result of
        Left  err -> printErr err >> return False
        Right g   ->
          return (any (\(Triple _ _ o) -> o == TLit (LInt 42)) (Set.toList g))

  , checkIO "base URI resolution (relative IRI)" $ do
      result <- parseTurtleFile "fixtures/smoke/tiny.ttl"
      case result of
        Left  err -> printErr err >> return False
        Right g   ->
          return (any (\(Triple s _ _) ->
            s == TIri "http://example.org/a") (Set.toList g))

  , checkIO "prefix expansion (prefixed name to full IRI)" $ do
      result <- parseTurtleFile "fixtures/smoke/tiny.ttl"
      case result of
        Left  err -> printErr err >> return False
        Right g   ->
          return (any (\(Triple _ p _) ->
            p == TIri "http://example.org/ont/name") (Set.toList g))
  ]
  where
    printErr err = putStrLn ("    error: " ++ show err)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== RenderSpec ==="
  runTests renderTests
  putStrLn "=== TurtleSpec ==="
  runTests turtleTests
