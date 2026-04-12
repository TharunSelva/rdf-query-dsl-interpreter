module RQL.Turtle.Loader (parseTurtleFile) where

import Control.Exception (ErrorCall, IOException, SomeException, displayException, evaluate, fromException, try)
import qualified Data.Set as Set
import RQL.Common.Errors (TurtleError (..), TurtleException (..))
import RQL.Common.Types  (Graph, Literal (..), Term (..), Triple (..))
import RQL.Turtle.Lexer  (alexScanTokens)
import RQL.Turtle.Lower  (lower)
import RQL.Turtle.Parser (parseTurtle)

parseTurtleFile :: FilePath -> IO (Either TurtleError Graph)
parseTurtleFile path = do
  contentsResult <- try (readContents path) :: IO (Either IOException String)
  case contentsResult of
    Left ex ->
      pure (Left (TurtleFileError path (displayException ex)))
    Right contents -> do
      graphResult <- try (do
        let graph = lower (parseTurtle (alexScanTokens contents))
        _ <- evaluate (forceGraph graph)
        pure graph) :: IO (Either SomeException Graph)
      pure (either (Left . classifyTurtleFailure) Right graphResult)

readContents :: FilePath -> IO String
readContents path = do
  contents <- readFile path
  _ <- evaluate (length contents)
  pure contents

classifyTurtleFailure :: SomeException -> TurtleError
classifyTurtleFailure ex =
  case fromException ex of
    Just turtleEx ->
      turtleExceptionToError turtleEx
    Nothing ->
      case fromException ex of
        Just errCall ->
          TurtleSyntaxError (displayException (errCall :: ErrorCall))
        Nothing ->
          TurtleSyntaxError (displayException ex)

turtleExceptionToError :: TurtleException -> TurtleError
turtleExceptionToError turtleEx =
  case turtleEx of
    TurtleSyntaxException msg ->
      TurtleSyntaxError msg
    TurtlePrefixException prefix ->
      TurtlePrefixError prefix

forceGraph :: Graph -> ()
forceGraph =
  Set.foldl' forceTriple ()

forceTriple :: () -> Triple -> ()
forceTriple () (Triple subject predicate objectTerm) =
  forceTerm subject `seq` forceTerm predicate `seq` forceTerm objectTerm

forceTerm :: Term -> ()
forceTerm term =
  case term of
    TIri iri ->
      length iri `seq` ()
    TLit literal ->
      forceLiteral literal

forceLiteral :: Literal -> ()
forceLiteral literal =
  case literal of
    LString value ->
      length value `seq` ()
    LInt value ->
      value `seq` ()
