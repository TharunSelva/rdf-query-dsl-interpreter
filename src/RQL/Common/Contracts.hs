module RQL.Common.Contracts where

import RQL.Common.Errors
import RQL.Common.Types
import RQL.Syntax.AST

parseTurtleFileContract :: FilePath -> IO (Either TurtleError Graph)
parseTurtleFileContract = error "Implemented in RQL.Turtle.Loader"

parseProgramFileContract :: FilePath -> IO (Either ParseError Program)
parseProgramFileContract = error "Implemented in RQL.Syntax.Parser"

runProgramContract :: Program -> IO (Either RuntimeError Graph)
runProgramContract = error "Implemented in RQL.Eval.Runtime"