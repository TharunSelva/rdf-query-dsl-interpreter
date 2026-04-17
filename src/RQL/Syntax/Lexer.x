{
module RQL.Syntax.Lexer
  ( Token (..)
  , alexScanTokens
  ) where
}

%wrapper "basic"

$digit   = [0-9]
$nonzero = [1-9]
$alpha   = [a-zA-Z]
$alnum   = [a-zA-Z0-9_]
$irichar = [^ \t\n\r\60\62\\]
$dquote  = [\34]
$strchar = [^\34\n\\]

tokens :-

[\ \t\n\r\f]+ ;
"#" [^\n]* ;

-- Keywords: must appear before TokIdent
"let"          { \_ -> TokLet }
"load"         { \_ -> TokLoad }
"print"        { \_ -> TokPrint }
"union"        { \_ -> TokUnion }
"intersection" { \_ -> TokIntersection }
"difference"   { \_ -> TokDifference }
"match"        { \_ -> TokMatch }
"filter"       { \_ -> TokFilter }
"where"        { \_ -> TokWhere }
"construct"    { \_ -> TokConstruct }
"from"         { \_ -> TokFrom }
"in"           { \_ -> TokIn }
"groupmax"     { \_ -> TokGroupMax }
"typeInfer"    { \_ -> TokTypeInfer }

-- Literals and references
"<" $irichar* ">"        { \s -> TokIriRef (tail (init s)) }
$dquote $strchar* $dquote { \s -> TokStringLit (tail (init s)) }

-- unsigned integers only - no leading 0s except the number 0 itself
"0"                      { \_ -> TokIntLit 0 }
$nonzero $digit*         { \s -> TokIntLit (read s) }

-- Variables: store the name without the leading '?'
"?" $alpha $alnum*       { \s -> TokVar (tail s) }

-- Identifiers
$alpha $alnum*           { \s -> TokIdent s }

-- Operators and punctuation
">="                     { \_ -> TokGe }
"=="                     { \_ -> TokEqEq }
"&&"                     { \_ -> TokAnd }
"||"                     { \_ -> TokOr }
"="                      { \_ -> TokEquals }
"{"                      { \_ -> TokLBrace }
"}"                      { \_ -> TokRBrace }
"("                      { \_ -> TokLParen }
")"                      { \_ -> TokRParen }
","                      { \_ -> TokComma }

{
data Token
  = TokLet
  | TokEquals
  | TokLoad
  | TokPrint
  | TokUnion
  | TokIntersection
  | TokDifference
  | TokMatch
  | TokFilter
  | TokWhere
  | TokConstruct
  | TokFrom
  | TokIn
  | TokGroupMax
  | TokTypeInfer
  | TokVar String
  | TokIriRef String
  | TokStringLit String
  | TokIntLit Int
  | TokIdent String
  | TokLBrace
  | TokRBrace
  | TokLParen
  | TokRParen
  | TokComma
  | TokGe
  | TokEqEq
  | TokAnd
  | TokOr
  deriving (Eq, Show)
}