{
module RQL.Syntax.Parser (parseProgramFile) where

import RQL.Common.Errors
import RQL.Syntax.AST
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%%

Program :: { Program }
        : { Program [] }

{
data Token = DummyToken
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Program parse error"

parseProgramFile :: FilePath -> IO (Either ParseError Program)
parseProgramFile _ = pure (Right (Program []))
}
