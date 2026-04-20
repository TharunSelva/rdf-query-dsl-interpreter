module RQL.Eval.Runtime (runProgram) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub)
import Data.Maybe (mapMaybe)

import RQL.Common.Errors
import RQL.Common.Types
import RQL.Syntax.AST
import RQL.Eval.GraphOps
import RQL.Eval.Builtins
import RQL.Turtle.Loader (parseTurtleFile)

-- Environment and Store

type Env = Map.Map Name Value
type Store = Map.Map Name Graph

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Value -> Env -> Env
extendEnv = Map.insert

lookupEnv :: Name -> Env -> Value
lookupEnv x env = case Map.lookup x env of
  Just v -> v
  Nothing -> error ("RQL: unbound variable: " ++ x)

-- Actual interpreter

evalExpr :: Env -> Store -> Expr -> Graph
evalExpr env _store (EVar x) = case lookupEnv x env of
  VGraph g -> g
  _ -> error ("RQL: variable " ++ x ++ " is not a graph")

evalExpr _env _store (ELoad path) =
  error ("RQL: ELoad requires IO - use runProgram to evaluate: " ++ path)

evalExpr env store (EUnion e1 e2) =
  graphUnion (evalExpr env store e1) (evalExpr env store e2)

evalExpr env store (EIntersection e1 e2) =
  graphIntersection (evalExpr env store e1) (evalExpr env store e2)

evalExpr env store (EDifference e1 e2) =
  graphDifference (evalExpr env store e1) (evalExpr env store e2)

evalExpr _env store (EMatch clauses) =
  let matchedTriples (GraphPattern gname pat) =
        case Map.lookup gname store of
          Just g -> Set.filter (maybe False (const True) . matchTriple pat) g
          Nothing -> error ("RQL match: unknown graph: " ++ gname)
  in Set.unions (map matchedTriples clauses)

evalExpr env store (EFilter pred_ e) =
  let g = evalExpr env store e
      testTriple t =
        let b = Map.fromList [("s", subject t), ("p", predicate t), ("o", object t)]
        in evalPred pred_ b
  in Set.filter testTriple g

evalExpr env store (EConstruct pats e) =
  let g = evalExpr env store e
      bindTriple t = Map.fromList [("s", subject t), ("p", predicate t), ("o", object t)]
      newTriples = [instantiateTriple pat (bindTriple t) | t <- Set.toList g, pat <- pats]
  in Set.fromList (nub newTriples)

evalExpr env store (EGroupMax e groupVar maxIri) =
  groupMax groupVar maxIri (evalExpr env store e)

evalExpr env store (EGroupMin e groupVar minIri) =
  groupMin groupVar minIri (evalExpr env store e)

evalExpr env store (EChain e1 e2) =
  chainGraphs (evalExpr env store e1) (evalExpr env store e2)

evalExpr env store (ETypeInfer e1 e2) =
  inferTypes (evalExpr env store e1) (evalExpr env store e2)

-- Statement execution

execStmt :: Env -> Store -> Stmt -> (Env, [Graph])
execStmt env store (SLet x e) =
  let g = evalExpr env store e
  in (extendEnv x (VGraph g) env, [])
execStmt env store (SPrint e) =
  let g = evalExpr env store e
  in (env, [g])

execProgram :: Store -> Program -> [Graph]
execProgram store (Program stmts) =
  snd $ foldl step (emptyEnv, []) stmts
  where
    step (env, acc) stmt =
      let (env', printed) = execStmt env store stmt
      in (env', acc ++ printed)

-- IO shell

collectLoads :: Program -> [(VarName, GraphName)]
collectLoads (Program stmts) = concatMap fromStmt stmts
  where
    fromStmt (SLet x (ELoad path)) = [(x, path)]
    fromStmt _ = []

runProgram :: Program -> IO (Either RuntimeError Graph)
runProgram prog@(Program stmts) = do
  -- Pre-load all graphs
  loadResults <- mapM loadGraph (collectLoads prog)
  case sequence loadResults of
    Left err -> pure (Left err)
    Right storeList -> do
      let store = Map.fromList storeList
      -- Build initial env with loaded graphs
      let initEnv = foldl (\e (x, g) -> extendEnv x (VGraph g) e) emptyEnv storeList
      -- Execute
      let results = execProgram store prog
      -- For now, return the last printed graph or empty
      pure (Right (if null results then Set.empty else last results))
  where
    loadGraph (x, path) = do
      result <- parseTurtleFile path
      case result of
        Left err -> pure (Left (RuntimeError (show err)))
        Right g -> pure (Right (x, g))