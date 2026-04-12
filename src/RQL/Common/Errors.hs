module RQL.Common.Errors
  ( TurtleError (..)
  , TurtleException (..)
  , renderTurtleError
  , ParseError (..)
  , RuntimeError (..)
  ) where

import Control.Exception (Exception (displayException))

data TurtleError
  = TurtleFileError FilePath String
  | TurtleSyntaxError String
  | TurtlePrefixError String
  deriving (Eq, Show)

data TurtleException
  = TurtleSyntaxException String
  | TurtlePrefixException String
  deriving (Show)

instance Exception TurtleException where
  displayException (TurtleSyntaxException msg) = msg
  displayException (TurtlePrefixException prefix) =
    "undeclared prefix: " ++ prefix

renderTurtleError :: TurtleError -> String
renderTurtleError turtleError =
  case turtleError of
    TurtleFileError path msg ->
      "could not read '" ++ path ++ "': " ++ msg
    TurtleSyntaxError msg ->
      "invalid Turtle input: " ++ msg
    TurtlePrefixError prefix ->
      "undeclared prefix: " ++ prefix

data ParseError = ParseError String
  deriving (Eq, Show)

data RuntimeError = RuntimeError String
  deriving (Eq, Show)
