{
module RQL.Turtle.Parser where
}

%name parseTurtle
%tokentype { Token }
%error { parseError }

%%

TurtleDoc :: { () }
          : { () }

{
data Token = DummyToken
  deriving (Eq, Show)

parseError :: [Token] -> a
parseError _ = error "Turtle parse error"
}
