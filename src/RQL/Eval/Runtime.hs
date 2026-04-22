module RQL.Eval.Runtime (runProgram, runProgramFile) where

import Control.Exception (SomeException, displayException, evaluate, try)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl', nub)
import System.Directory (makeAbsolute, withCurrentDirectory)
import System.FilePath (takeDirectory, takeExtension, (<.>))

import RQL.Common.Errors
import RQL.Common.Types
import RQL.Eval.Builtins
import RQL.Eval.GraphOps
import RQL.Syntax.AST
import RQL.Syntax.Parser (parseProgramFile)
import RQL.Turtle.Loader (parseTurtleFile)
import RQL.Turtle.Render (renderGraph)

-- Environment and Store

type Env = Map.Map Name Value
type Store = Map.Map FilePath Graph

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Value -> Env -> Env
extendEnv = Map.insert

lookupEnv :: Name -> Env -> Value
lookupEnv x env = case Map.lookup x env of
  Just v -> v
  Nothing -> error ("RQL: unbound variable: " ++ x)

graphValue :: String -> Value -> Graph
graphValue _context (VGraph g) = g
graphValue context _ = error ("RQL: expected graph in " ++ context)

normalizeBindings :: [Binding] -> [Binding]
normalizeBindings = Set.toList . Set.fromList

bindingsUnion :: [Binding] -> [Binding] -> [Binding]
bindingsUnion left right = normalizeBindings (left ++ right)

bindingsIntersection :: [Binding] -> [Binding] -> [Binding]
bindingsIntersection left right =
  Set.toList (Set.intersection (Set.fromList left) (Set.fromList right))

bindingsDifference :: [Binding] -> [Binding] -> [Binding]
bindingsDifference left right =
  Set.toList (Set.difference (Set.fromList left) (Set.fromList right))

setOpValue :: String -> (Graph -> Graph -> Graph) -> ([Binding] -> [Binding] -> [Binding]) -> Value -> Value -> Value
setOpValue _context graphOp _bindingsOp (VGraph left) (VGraph right) =
  VGraph (graphOp left right)
setOpValue _context _graphOp bindingsOp (VBindings left) (VBindings right) =
  VBindings (bindingsOp left right)
setOpValue context _graphOp _bindingsOp _ _ =
  error ("RQL: " ++ context ++ " expects both graphs or both bindings")

-- Actual evaluator

evalExpr :: Env -> Store -> Expr -> Value
evalExpr env _store (EVar x) = lookupEnv x env

evalExpr _env store (ELoad path) =
  case Map.lookup path store of
    Just graph -> VGraph graph
    Nothing -> error ("RQL load: graph was not preloaded: " ++ path)

evalExpr env store (EUnion e1 e2) =
  setOpValue "union" graphUnion bindingsUnion (evalExpr env store e1) (evalExpr env store e2)

evalExpr env store (EIntersection e1 e2) =
  setOpValue "intersection" graphIntersection bindingsIntersection (evalExpr env store e1) (evalExpr env store e2)

evalExpr env store (EDifference e1 e2) =
  setOpValue "difference" graphDifference bindingsDifference (evalExpr env store e1) (evalExpr env store e2)

evalExpr env _store (EMatch clauses) =
  let matchClause (GraphPattern gname pat) =
        case Map.lookup gname env of
          Just (VGraph graph) -> matchGraph pat graph
          Just _ -> error ("RQL match: expected graph variable: " ++ gname)
          Nothing -> error ("RQL match: unknown graph: " ++ gname)
      joinClauseRelations [] = []
      joinClauseRelations (rel:rels) = foldl' joinBindings rel rels
  in VBindings (joinClauseRelations (map matchClause clauses))

evalExpr env store (EFilter pred_ e) =
  case evalExpr env store e of
    VBindings bindings ->
      VBindings (filterBindings pred_ bindings)
    VGraph _ -> error "RQL filter expects bindings"

evalExpr env store (EConstruct pats e) =
  case evalExpr env store e of
    VBindings bindings ->
      case instantiateGraph pats bindings of
        Left msg -> error msg
        Right graph -> VGraph graph
    VGraph _ -> error "RQL construct expects bindings"

evalExpr env store (EGroupMax e groupVar predicateTerm) =
  case groupMax groupVar predicateTerm (graphValue "groupmax" (evalExpr env store e)) of
    Left msg -> error msg
    Right graph -> VGraph graph

evalExpr env store (EGroupMin e groupVar minIri) =
  case groupMin groupVar minIri (graphValue "groupmin" (evalExpr env store e)) of
    Left msg -> error msg
    Right graph -> VGraph graph

evalExpr env store (EChain e1 e2) =
  VGraph (chainGraphs (graphValue "chain" (evalExpr env store e1)) (graphValue "chain" (evalExpr env store e2)))

evalExpr env store (ETypeInfer e1 e2) =
  VGraph (inferTypes (graphValue "typeInfer" (evalExpr env store e1)) (graphValue "typeInfer" (evalExpr env store e2)))

-- Statement execution

execStmt :: Env -> Store -> Stmt -> (Env, [Graph])
execStmt env store (SLet x e) =
  let value = evalExpr env store e
  in forceValue value `seq` (extendEnv x value env, [])
execStmt env store (SPrint e) =
  case evalExpr env store e of
    VGraph g -> forceGraph g `seq` (env, [g])
    VBindings _ -> error "RQL: print expects a graph"

execProgram :: Store -> Program -> [Graph]
execProgram store (Program stmts) =
  snd $ foldl' step (emptyEnv, []) stmts
  where
    step (env, acc) stmt =
      let (env', printed) = execStmt env store stmt
      in (env', acc ++ printed)

-- IO

collectLoads :: Program -> [FilePath]
collectLoads (Program stmts) = nub (concatMap fromStmt stmts)
  where
    fromStmt (SLet _ e) = fromExpr e
    fromStmt (SPrint e) = fromExpr e

    fromExpr (EVar _) = []
    fromExpr (ELoad path) = [path]
    fromExpr (EUnion e1 e2) = fromExpr e1 ++ fromExpr e2
    fromExpr (EIntersection e1 e2) = fromExpr e1 ++ fromExpr e2
    fromExpr (EDifference e1 e2) = fromExpr e1 ++ fromExpr e2
    fromExpr (EMatch _) = []
    fromExpr (EFilter _ e) = fromExpr e
    fromExpr (EConstruct _ e) = fromExpr e
    fromExpr (EGroupMax e _ _) = fromExpr e
    fromExpr (EGroupMin e _ _) = fromExpr e
    fromExpr (EChain e1 e2) = fromExpr e1 ++ fromExpr e2
    fromExpr (ETypeInfer e1 e2) = fromExpr e1 ++ fromExpr e2

runProgram :: Program -> IO (Either RuntimeError Graph)
runProgram prog = do
  loadResults <- mapM loadGraph (collectLoads prog)
  case sequence loadResults of
    Left err -> pure (Left err)
    Right storeList -> do
      let store = Map.fromList storeList
      execResult <- try (evaluate (runAndForce store prog)) :: IO (Either SomeException Graph)
      pure $
        case execResult of
          Left ex -> Left (RuntimeError (displayException ex))
          Right graph -> Right graph
  where
    loadGraph path = do
      result <- parseTurtleFile (resolveLoadPath path)
      case result of
        Left err -> pure (Left (RuntimeError (renderTurtleError err)))
        Right g -> pure (Right (path, g))

runProgramFile :: FilePath -> IO (Either String String)
runProgramFile path = do
  absolutePath <- makeAbsolute path
  parsed <- parseProgramFile absolutePath
  case parsed of
    Left (ParseError msg) ->
      pure (Left msg)
    Right program ->
      withCurrentDirectory (takeDirectory absolutePath) $ do
        result <- runProgram program
        pure $
          case result of
            Left (RuntimeError msg) -> Left msg
            Right graph -> Right (renderGraph graph)

resolveLoadPath :: FilePath -> FilePath
resolveLoadPath path
  | null (takeExtension path) = path <.> "ttl"
  | otherwise = path

runAndForce :: Store -> Program -> Graph
runAndForce store program =
  let results = execProgram store program
      graph = if null results then Set.empty else last results
  in forceGraph graph `seq` graph

forceValue :: Value -> ()
forceValue (VGraph graph) = forceGraph graph
forceValue (VBindings bindings) = foldl' forceBinding () bindings

forceBinding :: () -> Binding -> ()
forceBinding () binding = Map.foldl' forceBoundTerm () binding

forceGraph :: Graph -> ()
forceGraph = Set.foldl' forceTriple ()

forceTriple :: () -> Triple -> ()
forceTriple () (Triple s p o) =
  forceTerm s `seq` forceTerm p `seq` forceTerm o

forceBoundTerm :: () -> Term -> ()
forceBoundTerm () term = forceTerm term

forceTerm :: Term -> ()
forceTerm term =
  case term of
    TIri iri ->
      length iri `seq` ()
    TLit (LString value) ->
      length value `seq` ()
    TLit (LInt value) ->
      value `seq` ()

filterBindings :: Predicate -> [Binding] -> [Binding]
filterBindings pred_ = filter (evalPred pred_)
