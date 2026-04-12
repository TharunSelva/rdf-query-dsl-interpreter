{
{-# OPTIONS_GHC -w #-}
module RQL.Turtle.Parser (parseTurtle) where

import Control.Exception (throw)
import RQL.Common.Errors (TurtleException (TurtleSyntaxException))
import RQL.Common.Types  (Literal (..))
import RQL.Turtle.AST
import RQL.Turtle.Lexer  (Token (..))
}

%name      parseTurtle
%tokentype { Token }
%error     { parseError }

%token
  base     { TokBase }
  prefix   { TokPrefix }
  'a'      { TokA }
  iriref   { TokIriRef $$ }
  pname    { TokPNameLN $$ }
  strlit   { TokStringLit $$ }
  intlit   { TokIntLit $$ }
  '.'      { TokDot }
  ';'      { TokSemicolon }
  ','      { TokComma }

%%

TurtleDoc :: { TurtleDoc }
          : RevStmts  { TurtleDoc (reverse $1) }

RevStmts :: { [TStmt] }
         : {- empty -}    { [] }
         | RevStmts Stmt  { $2 : $1 }

Stmt :: { TStmt }
     : base iriref '.'          { TBase $2 }
     | prefix pname iriref '.'  { TPrefix (fst $2) $3 }
     | Triples '.'              { $1 }

Triples :: { TStmt }
        : Subject RevPredicateObjectList   { TTriples $1 (reverse $2) }

Subject :: { TSubject }
        : iriref   { TSIri $1 }
        | pname    { uncurry TSPName $1 }

RevPredicateObjectList :: { [TPredObjList] }
    : PredObj                              { [$1] }
    | RevPredicateObjectList ';' PredObj   { $3 : $1 }

PredObj :: { TPredObjList }
        : Predicate RevObjectList   { ($1, reverse $2) }

Predicate :: { TPred }
          : iriref   { TPIri $1 }
          | pname    { uncurry TPPName $1 }
          | 'a'      { TPA }

RevObjectList :: { [TObj] }
    : Obj                     { [$1] }
    | RevObjectList ',' Obj   { $3 : $1 }

Obj :: { TObj }
    : iriref   { TOIri $1 }
    | pname    { uncurry TOPName $1 }
    | strlit   { TOLit (LString $1) }
    | intlit   { TOLit (LInt    $1) }

{
parseError :: [Token] -> a
parseError [] =
  throw (TurtleSyntaxException "unexpected end of input")
parseError ts =
  throw (TurtleSyntaxException ("unexpected tokens near " ++ show (take 3 ts)))
}
