module RQL.Common.Errors where

data TurtleError = TurtleError String
  deriving (Eq, Show)

data ParseError = ParseError String
  deriving (Eq, Show)

data RuntimeError = RuntimeError String
  deriving (Eq, Show)