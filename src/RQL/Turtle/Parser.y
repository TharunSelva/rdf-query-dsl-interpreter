{
module RQL.Turtle.Parser
  ( parseTurtle
  , ParseError(..)
  ) where

import RQL.Turtle.Lexer (Token(..))
import RQL.Turtle.AST
}

%name       parseTurtleTokens
%tokentype  { Token }
%error      { parseError }

%token
  BASE        { TokBase }
  PREFIX      { TokPrefix }
  DOT         { TokDot }
  SEMI        { TokSemicolon }
  COMMA       { TokComma }
  URI         { TokUri $$ }
  PREFIXED    { TokPrefixed $$ _ }
  STRING      { TokString $$ }
  INT         { TokInt $$ }

%%

-- A Turtle document is a sequence of statements
TurtleDoc :: { TurtleDoc }
  : Stmts   { TurtleDoc (reverse $1) }

Stmts :: { [TStmt] }
  : {- empty -}    { [] }
  | Stmts Stmt     { $2 : $1 }

Stmt :: { TStmt }
  : BASE URI DOT                      { TBase $2 }
  | PREFIX PrefixedRaw URI DOT        { let (pre,_) = $2 in TPrefix pre $3 }
  | Subject PredObjList DOT           { TTriples $1 (reverse $2) }

PrefixedRaw :: { (String, String) }
  : PREFIXED    { $1 }   -- TOKEN already carries (prefix, local) pair

Subject :: { Subject }
  : URI                { SUri $1 }
  | PrefixedName       { uncurry SPref $1 }
  | RelUri             { SBaseRel $1 }

Predicate :: { Subject }
  : URI                { SUri $1 }
  | PrefixedName       { uncurry SPref $1 }
  | RelUri             { SBaseRel $1 }

RelUri :: { String }
  : URI   { $1 }

PrefixedName :: { (String, String) }
  : PREFIXED  { $1 }

PredObjList :: { [PredObjs] }
  : PredObjs                      { [$1] }
  | PredObjList SEMI PredObjs     { $3 : $1 }
  | PredObjList SEMI              { $1 }

PredObjs :: { PredObjs }
  : Predicate ObjList   { PredObjs $1 (reverse $2) }

ObjList :: { [Obj] }
  : Obj                 { [$1] }
  | ObjList COMMA Obj   { $3 : $1 }

Obj :: { Obj }
  : URI           { OUri $1 }
  | PrefixedName  { uncurry OPref $1 }
  | RelUri        { OBaseRel $1 }
  | STRING        { OStr $1 }
  | INT           { OInt $1 }

{
newtype ParseError = ParseError String deriving (Show)

parseError :: [Token] -> a
parseError []     = error "Parse error: unexpected end of input"
parseError (t:_)  = error ("Parse error at token: " ++ show t)

parseTurtle :: [Token] -> Either ParseError TurtleDoc
parseTurtle tokens =
  Right (parseTurtleTokens tokens)
}