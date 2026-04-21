module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import qualified Data.Set as Set
import RQL.Common.Errors (RuntimeError (..), TurtleError (..), renderTurtleError)
import RQL.Common.Types
import RQL.Eval.Runtime (runProgram, runProgramFile)
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
          ( \(Triple _ predicateTerm _) ->
              predicateTerm == TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
          )
          . Set.toList
  , checkIO "string literal (LF-only file)" $
      expectGraph "fixtures/smoke/stronly.ttl" (\graph -> Set.size graph == 1)
  , checkIO "integer literal parsed and rendered correctly" $
      expectGraph "fixtures/smoke/intlit.ttl" $
        any (\(Triple _ _ objectTerm) -> objectTerm == TLit (LInt 42)) . Set.toList
  , checkIO "base URI resolution (relative IRI)" $
      expectGraph "fixtures/smoke/tiny.ttl" $
        any (\(Triple subjectTerm _ _) -> subjectTerm == TIri "http://example.org/a") . Set.toList
  , checkIO "prefix expansion (prefixed name to full IRI)" $
      expectGraph "fixtures/smoke/tiny.ttl" $
        any (\(Triple _ predicateTerm _) -> predicateTerm == TIri "http://example.org/ont/name") . Set.toList
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
  , pure $
      checkPure "dsl lexer extended predicate operators"
        ( SL.alexScanTokens "?x != ?y && 3 < 4 && 5 > 6 && 7 <= 8 && ! (?x == ?y)"
            == [ SL.TokVar "x"
               , SL.TokNe
               , SL.TokVar "y"
               , SL.TokAnd
               , SL.TokIntLit 3
               , SL.TokLt
               , SL.TokIntLit 4
               , SL.TokAnd
               , SL.TokIntLit 5
               , SL.TokGt
               , SL.TokIntLit 6
               , SL.TokAnd
               , SL.TokIntLit 7
               , SL.TokLe
               , SL.TokIntLit 8
               , SL.TokAnd
               , SL.TokNot
               , SL.TokLParen
               , SL.TokVar "x"
               , SL.TokEqEq
               , SL.TokVar "y"
               , SL.TokRParen
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
  , checkIO "dsl parser supports not and new comparisons" $
      expectProgramText
        "let g = load \"g\"\n\
        \let m = match {\n\
        \in g { ?s ?p ?o }\n\
        \}\n\
        \let a = filter m where (!(?o <= 21) || ?p != <http://example.org/age>)\n\
        \let c = construct { ?s ?p ?o } from a\n\
        \print c\n"
        ( == Program
               [ SLet "g" (ELoad "g")
               , SLet
                   "m"
                   (EMatch [GraphPattern "g" (TriplePattern (PVar "s") (PVar "p") (PVar "o"))])
               , SLet
                   "a"
                   ( EFilter
                       ( POr
                           (PNot (PLe (PVar "o") (PConst (TLit (LInt 21)))))
                           (PNe (PVar "p") (PConst (TIri "http://example.org/age")))
                       )
                       (EVar "m")
                   )
               , SLet
                   "c"
                   (EConstruct [TriplePattern (PVar "s") (PVar "p") (PVar "o")] (EVar "a"))
               , SPrint (EVar "c")
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
               , SLet "g" (EGroupMax (EVar "quux") "s" (TIri "http://example.org/ont/price"))
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl parser task 5 program" $
      expectProgramText
        "let xyzzy = load \"xyzzy\"\n\
        \let plugh = load \"plugh\"\n\
        \let m = match {\n\
        \in xyzzy { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?y }\n\
        \in plugh { ?y <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?z }\n\
        \}\n\
        \let g = construct {\n\
        \?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?z\n\
        \} from m\n\
        \print g\n"
        ( == Program
               [ SLet "xyzzy" (ELoad "xyzzy")
               , SLet "plugh" (ELoad "plugh")
               , SLet
                   "m"
                   ( EMatch
                       [ GraphPattern
                           "xyzzy"
                           ( TriplePattern
                               (PVar "x")
                               (PConst (TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
                               (PVar "y")
                           )
                       , GraphPattern
                           "plugh"
                           ( TriplePattern
                               (PVar "y")
                               (PConst (TIri "http://www.w3.org/2000/01/rdf-schema#subClassOf"))
                               (PVar "z")
                           )
                       ]
                   )
               , SLet
                   "g"
                   ( EConstruct
                       [ TriplePattern
                           (PVar "x")
                           (PConst (TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
                           (PVar "z")
                       ]
                       (EVar "m")
                   )
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
  , checkIO "dsl file parser task5.rql uses joined DSL" $
      expectProgramFile "fixtures/smoke/task5.rql"
        ( == Program
               [ SLet "xyzzy" (ELoad "xyzzy")
               , SLet "plugh" (ELoad "plugh")
               , SLet
                   "m"
                   ( EMatch
                       [ GraphPattern
                           "xyzzy"
                           ( TriplePattern
                               (PVar "x")
                               (PConst (TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
                               (PVar "y")
                           )
                       , GraphPattern
                           "plugh"
                           ( TriplePattern
                               (PVar "y")
                               (PConst (TIri "http://www.w3.org/2000/01/rdf-schema#subClassOf"))
                               (PVar "z")
                           )
                       ]
                   )
               , SLet
                   "g"
                   ( EConstruct
                       [ TriplePattern
                           (PVar "x")
                           (PConst (TIri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))
                           (PVar "z")
                       ]
                       (EVar "m")
                   )
               , SPrint (EVar "g")
               ]
        )
  , checkIO "dsl file parser bad_syntax.rql fails" $
      expectParseFailureFile "fixtures/smoke/bad_syntax.rql"
  ]

runtimeTests :: [IO Result]
runtimeTests =
  [ checkIO "task 5 works through joined DSL without typeInfer" $
      expectProgramRun
        "let xyzzy = load \"fixtures/smoke/xyzzy\"\n\
        \let plugh = load \"fixtures/smoke/plugh\"\n\
        \let m = match {\n\
        \in xyzzy { ?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?y }\n\
        \in plugh { ?y <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?z }\n\
        \}\n\
        \let g = construct {\n\
        \?x <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> ?z\n\
        \} from m\n\
        \print g\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   (iriTerm "http://example.org/Person")
               , Triple
                   (iriTerm "http://example.org/bob")
                   (iriTerm "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   (iriTerm "http://example.org/Person")
               , Triple
                   (iriTerm "http://example.org/bob")
                   (iriTerm "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   (iriTerm "http://example.org/Worker")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   (iriTerm "http://example.org/Person")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
                   (iriTerm "http://example.org/Worker")
               ]
        )
  , checkIO "variables named x y z flow through join/filter/construct" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?x <http://example.org/knows> ?y }\n\
        \in g { ?y <http://example.org/age> ?z }\n\
        \}\n\
        \let a = filter m where (?z >= 21)\n\
        \let c = construct { ?x <http://example.org/seenBy> ?y } from a\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/seenBy")
                   (iriTerm "http://example.org/bob")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/seenBy")
                   (iriTerm "http://example.org/bob")
               ]
        )
  , checkIO "multi pattern join produces shared variable bindings" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?x <http://example.org/knows> ?y }\n\
        \in g { ?y <http://example.org/age> ?z }\n\
        \}\n\
        \let c = construct { ?x <http://example.org/seenBy> ?z } from m\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/seenBy")
                   (intTerm 21)
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/seenBy")
                   (intTerm 21)
               ]
        )
  , checkIO "three clause join preserves shared-variable agreement" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let h = load \"fixtures/smoke/binding\"\n\
        \let m = match {\n\
        \in g { ?x <http://example.org/knows> ?y }\n\
        \in h { ?x <http://example.org/knows> ?x }\n\
        \in g { ?y <http://example.org/age> ?z }\n\
        \}\n\
        \let c = construct { ?x <http://example.org/link> ?z } from m\n\
        \print c\n"
        ( == Set.singleton
               ( Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/link")
                   (intTerm 21)
               )
        )
  , checkIO "repeated variables must match the same term" $
      expectProgramRun
        "let g = load \"fixtures/smoke/binding\"\n\
        \let m = match {\n\
        \in g { ?node <http://example.org/knows> ?node }\n\
        \}\n\
        \let c = construct { ?node <http://example.org/seenBy> ?node } from m\n\
        \print c\n"
        ( == Set.singleton
               ( Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/seenBy")
                   (iriTerm "http://example.org/alice")
               )
        )
  , checkIO "empty match result constructs an empty graph" $
      expectProgramOutput
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?x <http://example.org/missing> ?y }\n\
        \}\n\
        \let c = construct { ?x <http://example.org/seenBy> ?y } from m\n\
        \print c\n"
        (== "")
  , checkIO "construct from joined bindings deduplicates duplicate templates" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?x <http://example.org/knows> ?y }\n\
        \in g { ?y <http://example.org/age> ?z }\n\
        \}\n\
        \let c = construct {\n\
        \?x <http://example.org/seenBy> ?z\n\
        \?x <http://example.org/seenBy> ?z\n\
        \} from m\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/seenBy")
                   (intTerm 21)
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/seenBy")
                   (intTerm 21)
               ]
        )
  , checkIO "match can consume a derived graph value" $
      expectProgramRun
        "let left = load \"fixtures/smoke/join\"\n\
        \let right = load \"fixtures/smoke/binding\"\n\
        \let both = union left right\n\
        \let m = match {\n\
        \in both { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let c = construct { ?x <http://example.org/knows> ?y } from m\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/alice")
               , Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               ]
        )
  , checkIO "filter drops rows with missing variables instead of failing" $
      expectProgramRun
        "let left = load \"fixtures/smoke/join\"\n\
        \let right = load \"fixtures/smoke/join\"\n\
        \let m1 = match {\n\
        \in left { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let m2 = match {\n\
        \in right { ?x <http://example.org/age> ?z }\n\
        \}\n\
        \let both = union m1 m2\n\
        \let a = filter both where (?y == <http://example.org/bob>)\n\
        \let c = construct { ?x <http://example.org/seenBy> ?y } from a\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/seenBy")
                   (iriTerm "http://example.org/bob")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/seenBy")
                   (iriTerm "http://example.org/bob")
               ]
        )
  , checkIO "construct drops rows with missing variables instead of failing" $
      expectProgramRun
        "let left = load \"fixtures/smoke/join\"\n\
        \let right = load \"fixtures/smoke/join\"\n\
        \let m1 = match {\n\
        \in left { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let m2 = match {\n\
        \in right { ?x <http://example.org/age> ?z }\n\
        \}\n\
        \let both = union m1 m2\n\
        \let c = construct { ?x <http://example.org/seenBy> ?y } from both\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/seenBy")
                   (iriTerm "http://example.org/bob")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/seenBy")
                   (iriTerm "http://example.org/bob")
               ]
        )
  , checkIO "construct drops rows where variable subject becomes a literal" $
      expectProgramOutput
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s <http://example.org/age> ?o }\n\
        \}\n\
        \let c = construct { ?o <http://example.org/tagged> ?s } from m\n\
        \print c\n"
        (== "")
  , checkIO "union works on bindings as well as graphs" $
      expectProgramRun
        "let left = load \"fixtures/smoke/join\"\n\
        \let right = load \"fixtures/smoke/binding\"\n\
        \let m1 = match {\n\
        \in left { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let m2 = match {\n\
        \in right { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let both = union m1 m2\n\
        \let c = construct { ?x <http://example.org/knows> ?y } from both\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/alice")
               , Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               ]
        )
  , checkIO "binding difference is exact set subtraction, not anti-join" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let left = match {\n\
        \in g { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let right = match {\n\
        \in g { ?x <http://example.org/knows> <http://example.org/bob> }\n\
        \}\n\
        \let diff = difference left right\n\
        \let c = construct { ?x <http://example.org/knows> ?y } from diff\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               ]
        )
  , checkIO "!=" $
      expectProgramRun
        "let left = load \"fixtures/smoke/join\"\n\
        \let right = load \"fixtures/smoke/binding\"\n\
        \let both = union left right\n\
        \let m = match {\n\
        \in both { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let a = filter m where (?x != ?y)\n\
        \let c = construct { ?x <http://example.org/knows> ?y } from a\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/alice")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               , Triple
                   (iriTerm "http://example.org/carol")
                   (iriTerm "http://example.org/knows")
                   (iriTerm "http://example.org/bob")
               ]
        )
  , checkIO "< on integers" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s <http://example.org/age> ?o }\n\
        \}\n\
        \let a = filter m where (?o < 21)\n\
        \let c = construct { ?s <http://example.org/age> ?o } from a\n\
        \print c\n"
        ( == Set.singleton
               ( Triple
                   (iriTerm "http://example.org/dana")
                   (iriTerm "http://example.org/age")
                   (intTerm 17)
               )
        )
  , checkIO "> on integers" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s <http://example.org/age> ?o }\n\
        \}\n\
        \let a = filter m where (?o > 17)\n\
        \let c = construct { ?s <http://example.org/age> ?o } from a\n\
        \print c\n"
        ( == Set.singleton
               ( Triple
                   (iriTerm "http://example.org/bob")
                   (iriTerm "http://example.org/age")
                   (intTerm 21)
               )
        )
  , checkIO "<= on integers" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s <http://example.org/age> ?o }\n\
        \}\n\
        \let a = filter m where (?o <= 21)\n\
        \let c = construct { ?s <http://example.org/age> ?o } from a\n\
        \print c\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/bob")
                   (iriTerm "http://example.org/age")
                   (intTerm 21)
               , Triple
                   (iriTerm "http://example.org/dana")
                   (iriTerm "http://example.org/age")
                   (intTerm 17)
               ]
        )
  , checkIO "logical not on a predicate" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s <http://example.org/age> ?o }\n\
        \}\n\
        \let a = filter m where (!(?o < 21))\n\
        \let c = construct { ?s <http://example.org/age> ?o } from a\n\
        \print c\n"
        ( == Set.singleton
               ( Triple
                   (iriTerm "http://example.org/bob")
                   (iriTerm "http://example.org/age")
                   (intTerm 21)
               )
        )
  , checkIO "!(?x == ...)" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s <http://example.org/age> ?o }\n\
        \}\n\
        \let a = filter m where (!(?s == <http://example.org/dana>))\n\
        \let c = construct { ?s <http://example.org/age> ?o } from a\n\
        \print c\n"
        ( == Set.singleton
               ( Triple
                   (iriTerm "http://example.org/bob")
                   (iriTerm "http://example.org/age")
                   (intTerm 21)
               )
        )
  , checkIO "invalid ordered comparisons are safe and just filter out rows" $
      expectProgramOutput
        "let g = load \"fixtures/smoke/tiny\"\n\
        \let m = match {\n\
        \in g { ?s ?p ?o }\n\
        \}\n\
        \let a = filter m where (?o >= 21)\n\
        \let c = construct { ?s ?p ?o } from a\n\
        \print c\n"
        (== "")
  , checkIO "construct rejects literal subjects" $
      expectProgramErrorContains
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let c = construct { \"bad\" <http://example.org/seenBy> ?y } from m\n\
        \print c\n"
        "subject must be an IRI"
  , checkIO "construct rejects literal predicates" $
      expectProgramErrorContains
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?x <http://example.org/knows> ?y }\n\
        \}\n\
        \let c = construct { ?x \"bad\" ?y } from m\n\
        \print c\n"
        "predicate must be an IRI"
  , checkIO "task 1 style union still works" $
      expectProgramRun
        "let left = load \"fixtures/smoke/join\"\n\
        \let right = load \"fixtures/smoke/binding\"\n\
        \let both = union left right\n\
        \print both\n"
        ( == Set.fromList
               [ Triple (iriTerm "http://example.org/alice") (iriTerm "http://example.org/knows") (iriTerm "http://example.org/alice")
               , Triple (iriTerm "http://example.org/alice") (iriTerm "http://example.org/knows") (iriTerm "http://example.org/bob")
               , Triple (iriTerm "http://example.org/bob") (iriTerm "http://example.org/age") (intTerm 21)
               , Triple (iriTerm "http://example.org/carol") (iriTerm "http://example.org/knows") (iriTerm "http://example.org/bob")
               , Triple (iriTerm "http://example.org/dana") (iriTerm "http://example.org/age") (intTerm 17)
               ]
        )
  , checkIO "task 2 style program still works" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s <http://example.org/age> ?o }\n\
        \}\n\
        \let a = filter m where (?o >= 21)\n\
        \let c = construct { ?s <http://example.org/age> ?o } from a\n\
        \print c\n"
        ( == Set.singleton
               ( Triple
                   (iriTerm "http://example.org/bob")
                   (iriTerm "http://example.org/age")
                   (intTerm 21)
               )
        )
  , checkIO "task 3 style program still works" $
      expectProgramRun
        "let g = load \"fixtures/smoke/join\"\n\
        \let m = match {\n\
        \in g { ?s ?p ?o }\n\
        \}\n\
        \let c = construct { ?s ?p ?o } from m\n\
        \print c\n"
        ( == Set.fromList
               [ Triple (iriTerm "http://example.org/alice") (iriTerm "http://example.org/knows") (iriTerm "http://example.org/bob")
               , Triple (iriTerm "http://example.org/bob") (iriTerm "http://example.org/age") (intTerm 21)
               , Triple (iriTerm "http://example.org/carol") (iriTerm "http://example.org/knows") (iriTerm "http://example.org/bob")
               , Triple (iriTerm "http://example.org/dana") (iriTerm "http://example.org/age") (intTerm 17)
               ]
        )
  , checkIO "task 4 style groupmax keeps max integer price per subject" $
      expectProgramRun
        "let quux = load \"quux\"\n\
        \let g = groupmax quux ?s <http://example.org/ont/price>\n\
        \print g\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/book1")
                   (iriTerm "http://example.org/ont/price")
                   (intTerm 15)
               , Triple
                   (iriTerm "http://example.org/book2")
                   (iriTerm "http://example.org/ont/price")
                   (intTerm 7)
               ]
        )
  , checkIO "groupmax ignores non matching predicates" $
      expectProgramOutput
        "let quux = load \"quux\"\n\
        \let g = groupmax quux ?s <http://example.org/ont/price>\n\
        \print g\n"
        ( \output ->
            not ("<http://example.org/ont/name>" `isInfixOf` output)
              && output
              == "<http://example.org/book1> <http://example.org/ont/price> 15 .\n\
                 \<http://example.org/book2> <http://example.org/ont/price> 7 .\n"
        )
  , checkIO "groupmax output is unchanged by duplicate input triples" $
      expectProgramOutput
        "let quux = load \"quux\"\n\
        \let g = groupmax quux ?s <http://example.org/ont/price>\n\
        \print g\n"
        ( == "<http://example.org/book1> <http://example.org/ont/price> 15 .\n\
               \<http://example.org/book2> <http://example.org/ont/price> 7 .\n"
        )
  , checkIO "groupmin over integers still works" $
      expectProgramRun
        "let g = load \"fixtures/smoke/grouping\"\n\
        \let m = groupmin g ?s <http://example.org/ont/price>\n\
        \print m\n"
        ( == Set.fromList
               [ Triple
                   (iriTerm "http://example.org/book1")
                   (iriTerm "http://example.org/ont/price")
                   (intTerm 10)
               , Triple
                   (iriTerm "http://example.org/book2")
                   (iriTerm "http://example.org/ont/price")
                   (intTerm 7)
               ]
        )
  , checkIO "groupmax requires integer objects for the selected predicate" $
      expectProgramErrorContains
        "let g = load \"fixtures/smoke/grouping\"\n\
        \let m = groupmax g ?s <http://example.org/ont/name>\n\
        \print m\n"
        "expected an integer object"
  , checkIO "groupmax rejects grouping variables other than ?s" $
      expectProgramErrorContains
        "let g = load \"fixtures/smoke/grouping\"\n\
        \let m = groupmax g ?p <http://example.org/ont/price>\n\
        \print m\n"
        "only supports grouping by ?s"
  , checkIO "task 4 program file resolves sibling quux.ttl and returns canonical output" $
      expectProgramFileOutput "t4.rql"
        ( == "<http://example.org/book1> <http://example.org/ont/price> 15 .\n\
               \<http://example.org/book2> <http://example.org/ont/price> 7 .\n"
        )
  , checkIO "program file runner resolves sibling Turtle files and returns only canonical output" $ do
      expected <- readFile "fixtures/smoke/task5.expected"
      expectProgramFileOutput "fixtures/smoke/task5.rql" (== expected)
  ]

expectGraph :: FilePath -> (Graph -> Bool) -> IO Bool
expectGraph path check = do
  result <- parseTurtleFile path
  case result of
    Left err -> do
      printErr err
      pure False
    Right graph ->
      pure (check graph)

expectError :: FilePath -> (TurtleError -> Bool) -> IO Bool
expectError path check = do
  result <- parseTurtleFile path
  case result of
    Left err ->
      pure (check err)
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
expectProgramText src check = do
  result <- parseProgramText src
  case result of
    Left msg -> do
      putStrLn ("    parse error: " ++ msg)
      pure False
    Right program ->
      pure (check program)

expectProgramFile :: FilePath -> (Program -> Bool) -> IO Bool
expectProgramFile path check = do
  result <- SP.parseProgramFile path
  case result of
    Left err -> do
      putStrLn ("    parse error: " ++ show err)
      pure False
    Right program ->
      pure (check program)

expectParseFailureFile :: FilePath -> IO Bool
expectParseFailureFile path = do
  result <- SP.parseProgramFile path
  case result of
    Left _ -> pure True
    Right _ -> pure False

expectProgramRun :: String -> (Graph -> Bool) -> IO Bool
expectProgramRun src check = do
  result <- parseProgramText src
  case result of
    Left msg -> do
      putStrLn ("    parse error: " ++ msg)
      pure False
    Right program -> do
      runResult <- runProgram program
      case runResult of
        Left err -> do
          putStrLn ("    runtime error: " ++ show err)
          pure False
        Right graph ->
          pure (check graph)

expectProgramOutput :: String -> (String -> Bool) -> IO Bool
expectProgramOutput src check = do
  result <- parseProgramText src
  case result of
    Left msg -> do
      putStrLn ("    parse error: " ++ msg)
      pure False
    Right program -> do
      runResult <- runProgram program
      case runResult of
        Left err -> do
          putStrLn ("    runtime error: " ++ show err)
          pure False
        Right graph ->
          pure (check (renderGraph graph))

expectProgramErrorContains :: String -> String -> IO Bool
expectProgramErrorContains src expectedFragment = do
  result <- parseProgramText src
  case result of
    Left msg -> do
      putStrLn ("    parse error: " ++ msg)
      pure False
    Right program -> do
      runResult <- runProgram program
      case runResult of
        Left (RuntimeError msg) ->
          pure (expectedFragment `isInfixOf` msg)
        Right graph -> do
          putStrLn ("    unexpected graph: " ++ renderGraph graph)
          pure False

expectProgramFileOutput :: FilePath -> (String -> Bool) -> IO Bool
expectProgramFileOutput path check = do
  result <- runProgramFile path
  case result of
    Left msg -> do
      putStrLn ("    file runner error: " ++ msg)
      pure False
    Right output ->
      pure (check output)

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
  putStrLn "=== RuntimeSpec ==="
  runTests runtimeTests
