module Main (main) where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [programPath] ->
      putStrLn ("Stub runner: would execute " ++ programPath)
    _ ->
      putStrLn "Usage: stack exec -- rdf-query-dsl-interpreter path/to/program.rql"
