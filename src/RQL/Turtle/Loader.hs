module RQL.Turtle.Loader (parseTurtleFile) where

import Control.Exception  (SomeException, try, evaluate)
import RQL.Common.Errors  (TurtleError (..))
import RQL.Common.Types   (Graph)
import RQL.Turtle.Lexer   (alexScanTokens)
import RQL.Turtle.Parser  (parseTurtle)
import RQL.Turtle.Lower   (lower)

-- | Read a Turtle file from disk and return the resulting 'Graph', or a
-- 'TurtleError' if lexing, parsing, or prefix resolution fails.
parseTurtleFile :: FilePath -> IO (Either TurtleError Graph)
parseTurtleFile path = do
  contents <- readFile path
  result   <- try (evaluate (lower (parseTurtle (alexScanTokens contents))))
  case result of
    Left  ex -> return (Left  (TurtleError (show (ex :: SomeException))))
    Right g  -> return (Right g)