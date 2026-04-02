{
module RQL.Syntax.Lexer where
}

%wrapper "basic"

$white = [\ \t\n\r]

tokens :-

$white+ ;

{
data Token = DummyToken
  deriving (Eq, Show)
}
