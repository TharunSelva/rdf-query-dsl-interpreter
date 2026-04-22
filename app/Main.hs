module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import RQL.Eval.Runtime (runProgramFile)

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    -- [path] -> do
    --   result <- runProgramFile path
    [path] ->
      runAndMaybeWrite path Nothing
    [path, outPath] ->
      runAndMaybeWrite path (Just outPath)
    _ -> do
      hPutStrLn stderr ("Usage: " ++ progName ++ " path/to/file.rql [output-file]")
      exitFailure

runAndMaybeWrite :: FilePath -> Maybe FilePath -> IO ()
runAndMaybeWrite path maybeOutPath = do
  result <- runProgramFile path
  case result of
    Left msg -> do
      hPutStrLn stderr msg
      exitFailure
    Right output -> do
      putStr output
      case maybeOutPath of
        Nothing -> pure ()
        Just outPath -> writeFile outPath output