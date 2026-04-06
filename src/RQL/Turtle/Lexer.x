{
module RQL.Turtle.Lexer
  ( Token(..)
  , lexTurtle
  , LexError(..)
  ) where
}

%wrapper "basic"

$digit      = [0-9]
$alpha      = [a-zA-Z]
$namestart  = [$alpha \_]
$namechar   = [$alpha $digit \_ \- \.]
$urichar    = [^ \< \> \" \{ \} \| \\ \^ \` \  \t\n\r]
$strchar    = [^ \" \\]

tokens :-
  $white+                           ;
  "#" [^\n]*                        ;

  
  "@base"                           { \_ -> TokenBase }
  "@prefix"                         { \_ -> TokenPrefix }
  "."                               { \_ -> TokenDot }
  ";"                               { \_ -> TokenSemicolon }
  ","                               { \_ -> TokenComma }
  "<" $urichar* ">"                 { \s -> TokenUri (init (tail s)) }
  $namestart $namechar* ":" $namestart $namechar*
                                    { \s -> let (pre, rest) = break (==':') s
                                            in TokenPrefixed pre (tail rest) }
  $namestart $namechar* ":"         { \s -> TokenPrefixed (init s) "" }
  \" $strchar* \"                   { \s -> TokenString (init (tail s)) }
  (\-|"+")? $digit+                 { \s -> TokenInt (read s) }

{

data Token
  = TokenBase
  | TokenPrefix
  | TokenDot
  | TokenSemicolon
  | TokenComma
  | TokenUri String
  | TokenPrefixed String String
  | TokenString String
  | TokenInt Int
  | TokenEOF
  deriving (Eq, Show)

newtype LexError = LexError String deriving (Show)

lexTurtle :: String -> Either LexError [Token]
lexTurtle input =
  case alexScanTokens input of
    tokens -> Right tokens

}