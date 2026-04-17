module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless)
import qualified Data.Set as Set
import RQL.Common.Errors (TurtleError (..), renderTurtleError)
import RQL.Common.Types
import qualified RQL.Syntax.Lexer as SL
import qualified RQL.Syntax.Parser as SP
import RQL.Syntax.AST
import RQL.Turtle.Loader (parseTurtleFile)
import RQL.Turtle.Render (renderGraph)

data Result = Pass String | Fail String String
  deriving (Show)

checkPure :: String -> Bool -> Result
checkPure name True = Pass name
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
  [ pure $
      checkPure "render empty graph"
        (renderGraph Set.empty == "")
  , pure $
      checkPure "render single IRI triple"
        ( renderGraph (Set.singleton t1)
            == "<http://a.org/s> <http://a.org/p> \"hello\" .\n"
        )
  , pure $
      checkPure "render integer literal"
        ( renderGraph (Set.singleton t2)
            == "<http://a.org/s> <http://a.org/p> 99 .\n"
        )
  , pure $
      checkPure "render sorts triples lexicographically"
        ( renderGraph (Set.fromList [tb, ta])
            == "<http://a.org/s> <http://p.org/p> <http://o.org/o> .\n\
               \<http://b.org/s> <http://p.org/p> <http://o.org/o> .\n"
        )
  , pure $
      checkPure "render deduplicates identical triples"
        (Set.size (Set.fromList [t1, t1]) == 1)
  , pure $
      checkPure "render orders literals before IRIs when rendered text demands it"
        ( renderGraph
            ( Set.fromList
                [ Triple
                    (TIri "http://a.org/s")
                    (TIri "http://a.org/p")
                    (TIri "http://z.org/o")
                , Triple
                    (TIri "http://a.org/s")
                    (TIri "http://a.org/p")
                    (TLit (LString "hello"))
                ]
            )
            == "<http://a.org/s> <http://a.org/p> \"hello\" .\n\
               \<http://a.org/s> <http://a.org/p> <http://z.org/o> .\n"
        )
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
        any
          ( \(Triple _ predicate _) ->
              predicate == TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
          )
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

syntaxTests :: [IO Result]
syntaxTests =
  [ pure $
      checkPure "dsl lexer tiny let/load line"
        ( SL.alexScanTokens "let foo = load \"bar\""
            == [ SL.TokLet
               , SL.TokIdent "foo"
               , SL.TokEquals
               , SL.TokLoad
               , SL.TokStringLit "bar"
               ]
        )
  , checkIO "dsl parser trivial tiny program" $
      expectProgramText
        "let g = load \"tiny\"\nprint g"
        ( == Program
               [ SLet "g" (ELoad "tiny")
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl parser task 1 program" $
      expectProgramText
        "let foo = load \"foo\"\n\
        \let bar = load \"bar\"\n\
        \let both = union foo bar\n\
        \print both\n"
        ( == Program
               [ SLet "foo" (ELoad "foo")
               , SLet "bar" (ELoad "bar")
               , SLet "both" (EUnion (EVar "foo") (EVar "bar"))
               , SPrint (EVar "both")
               ]
        )
  , checkIO "dsl parser task 2 program" $
      expectProgramText
        "let baz = load \"baz\"\n\
        \let m = match {\n\
        \in baz { ?s <http://example.org/ont/hasAge> ?o }\n\
        \}\n\
        \let a = filter m where (?o >= 21)\n\
        \let g = construct {\n\
        \?s <http://example.org/ont/hasAge> ?o\n\
        \} from a\n\
        \print g\n"
        ( == Program
               [ SLet "baz" (ELoad "baz")
               , SLet
                   "m"
                   ( EMatch
                       [ GraphPattern
                           "baz"
                           ( TriplePattern
                               (PVar "s")
                               (PConst (TIri "http://example.org/ont/hasAge"))
                               (PVar "o")
                           )
                       ]
                   )
               , SLet
                   "a"
                   ( EFilter
                       (PGe (PVar "o") (PConst (TLit (LInt 21))))
                       (EVar "m")
                   )
               , SLet
                   "g"
                   ( EConstruct
                       [ TriplePattern
                           (PVar "s")
                           (PConst (TIri "http://example.org/ont/hasAge"))
                           (PVar "o")
                       ]
                       (EVar "a")
                   )
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl parser task 3 program" $
      expectProgramText
        "let qux = load \"qux\"\n\
        \let m = match {\n\
        \in qux { ?s ?p ?o }\n\
        \}\n\
        \let a = filter m where ((?p == <http://example.org/ont/studiesAt> || ?p == <http://example.org/ont/worksFor>) && ?o == <http://example.org/uos>)\n\
        \let g = construct { ?s ?p ?o } from a\n\
        \print g\n"
        ( == Program
               [ SLet "qux" (ELoad "qux")
               , SLet
                   "m"
                   ( EMatch
                       [ GraphPattern "qux" (TriplePattern (PVar "s") (PVar "p") (PVar "o"))
                       ]
                   )
               , SLet
                   "a"
                   ( EFilter
                       ( PAnd
                           ( POr
                               (PEq (PVar "p") (PConst (TIri "http://example.org/ont/studiesAt")))
                               (PEq (PVar "p") (PConst (TIri "http://example.org/ont/worksFor")))
                           )
                           (PEq (PVar "o") (PConst (TIri "http://example.org/uos")))
                       )
                       (EVar "m")
                   )
               , SLet
                   "g"
                   ( EConstruct
                       [TriplePattern (PVar "s") (PVar "p") (PVar "o")]
                       (EVar "a")
                   )
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl parser task 4 program" $
      expectProgramText
        "let quux = load \"quux\"\n\
        \let g = groupmax quux ?s <http://example.org/ont/price>\n\
        \print g\n"
        ( == Program
               [ SLet "quux" (ELoad "quux")
               , SLet "g" (EGroupMax (EVar "quux") "s" "http://example.org/ont/price")
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl parser task 5 program" $
      expectProgramText
        "let xyzzy = load \"xyzzy\"\n\
        \let plugh = load \"plugh\"\n\
        \let g = typeInfer(xyzzy, plugh)\n\
        \print g\n"
        ( == Program
               [ SLet "xyzzy" (ELoad "xyzzy")
               , SLet "plugh" (ELoad "plugh")
               , SLet "g" (ETypeInfer (EVar "xyzzy") (EVar "plugh"))
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl file parser tiny.rql succeeds" $
      expectProgramFile "fixtures/smoke/tiny.rql"
        ( == Program
               [ SLet "g" (ELoad "tiny")
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl file parser bad_syntax.rql fails" $
      expectParseFailureFile "fixtures/smoke/bad_syntax.rql"
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

parseProgramText :: String -> IO (Either String Program)
parseProgramText src = do
  result <- try (evaluate (SP.parseProgram (SL.alexScanTokens src))) :: IO (Either SomeException Program)
  pure $
    case result of
      Left ex -> Left (show ex)
      Right program -> Right program

expectProgramText :: String -> (Program -> Bool) -> IO Bool
expectProgramText src predicate = do
  result <- parseProgramText src
  case result of
    Left msg -> do
      putStrLn ("    parse error: " ++ msg)
      pure False
    Right program ->
      pure (predicate program)

expectProgramFile :: FilePath -> (Program -> Bool) -> IO Bool
expectProgramFile path predicate = do
  result <- SP.parseProgramFile path
  case result of
    Left err -> do
      putStrLn ("    parse error: " ++ show err)
      pure False
    Right program ->
      pure (predicate program)

expectParseFailureFile :: FilePath -> IO Bool
expectParseFailureFile path = do
  result <- SP.parseProgramFile path
  case result of
    Left _ -> pure True
    Right _ -> pure False

printErr :: TurtleError -> IO ()
printErr err = putStrLn ("    error: " ++ renderTurtleError err)

main :: IO ()
main = do
  putStrLn "=== RenderSpec ==="
  runTests renderTests
  putStrLn "=== TurtleSpec ==="
  runTests turtleTests
  putStrLn "=== SyntaxSpec ==="
  runTests syntaxTests
