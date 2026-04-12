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
