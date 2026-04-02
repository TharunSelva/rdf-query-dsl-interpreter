module RQL.Eval.Runtime (runProgram) where

import RQL.Common.Errors
import RQL.Common.Types
import RQL.Syntax.AST

runProgram :: Program -> IO (Either RuntimeError Graph)
runProgram _ = pure (Left (RuntimeError "Not implemented yet"))
