module Main (main) where

import Control.Exception (IOException, displayException, try)
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
    _ ->
      failWith ("Usage: " ++ progName ++ " path/to/file.rql [output-file]")

runAndMaybeWrite :: FilePath -> Maybe FilePath -> IO ()
runAndMaybeWrite path maybeOutPath = do
  result <- runProgramFile path
  case result of
    Left msg ->
      failWith msg
    Right output -> do
      putStr output
      case maybeOutPath of
        Nothing ->
          pure ()
        --old unsafe version:
        -- writeFile outPath output
        Just outPath -> do
          writeResult <- try (writeFile outPath output) :: IO (Either IOException ())
          case writeResult of
            Left ex ->
              failWith ("could not write '" ++ outPath ++ "': " ++ displayException ex)
            Right () ->
              pure ()

failWith :: String -> IO ()
failWith msg = do
  hPutStrLn stderr msg
  exitFailure