-- Temporary Role 1 entrypoint: Turtle-only validation harness.
-- This is NOT the final .rql interpreter CLI. It only loads and renders
-- a single .ttl file to verify the Turtle front-end pipeline.
-- The final entrypoint will accept a .rql program file once Role 2
-- (DSL parser) and Role 3 (evaluator) are integrated.
module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)
import RQL.Common.Errors  (renderTurtleError)
import RQL.Turtle.Loader  (parseTurtleFile)
import RQL.Turtle.Render  (renderGraph)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [path] ->
      runFile path
    _ -> do
      hPutStrLn stderr ("Usage: " ++ progName ++ " path/to/file.ttl")
      exitFailure

runFile :: FilePath -> IO ()
runFile path = do
  result <- parseTurtleFile path
  case result of
    Left err -> do
      hPutStrLn stderr (renderTurtleError err)
      exitFailure
    Right graph ->
      putStr (renderGraph graph)
