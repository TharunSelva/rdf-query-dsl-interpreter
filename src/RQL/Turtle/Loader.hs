module RQL.Turtle.Loader (parseTurtleFile) where

import RQL.Common.Errors
import RQL.Common.Types

parseTurtleFile :: FilePath -> IO (Either TurtleError Graph)
parseTurtleFile _ = pure (Left (TurtleError "Not implemented yet"))