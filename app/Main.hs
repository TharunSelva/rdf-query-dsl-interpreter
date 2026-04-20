module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import RQL.Common.Errors (ParseError (..), RuntimeError (..))
import RQL.Eval.Runtime (runProgram)
import RQL.Syntax.Parser (parseProgramFile)
import RQL.Turtle.Render (renderGraph)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [programPath] -> runProgramFile programPath
    _ -> do
      hPutStrLn stderr ("Usage: " ++ progName ++ " path/to/program.rql")
      exitFailure

runProgramFile :: FilePath -> IO ()
runProgramFile programPath = do
  parsed <- parseProgramFile programPath
  case parsed of
    Left (ParseError msg) -> do
      hPutStrLn stderr msg
      exitFailure
    Right program -> do
      result <- runProgram program
      case result of
        Left (RuntimeError msg) -> do
          hPutStrLn stderr msg
          exitFailure
        Right graph ->
          putStr (renderGraph graph)