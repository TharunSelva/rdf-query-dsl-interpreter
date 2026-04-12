{
{-# OPTIONS_GHC -w #-}
module RQL.Turtle.Parser (parseTurtle) where

import RQL.Turtle.Lexer  (Token (..))
import RQL.Turtle.AST
import RQL.Common.Types  (Literal (..))
}

%name      parseTurtle
%tokentype { Token }
%error     { parseError }

-- ---------------------------------------------------------------------------
-- Token declarations
-- ($$  binds the semantic value; _ is a wildcard for unit-valued tokens)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Grammar
-- ---------------------------------------------------------------------------

TurtleDoc :: { TurtleDoc }
          : Stmts   { TurtleDoc $1 }

-- Left-recursive list (avoids stack growth on large files)
Stmts :: { [TStmt] }
      : {- empty -}   { [] }
      | Stmts Stmt    { $1 ++ [$2] }

Stmt :: { TStmt }
     : base iriref '.'          { TBase $2 }
     | prefix pname iriref '.'  { TPrefix (fst $2) $3 }
     | Triples '.'              { $1 }

Triples :: { TStmt }
        : Subject PredicateObjectList   { TTriples $1 $2 }

Subject :: { TSubject }
        : iriref   { TSIri $1 }
        | pname    { uncurry TSPName $1 }

-- Left-recursive predicate-object list; ';'-separated
PredicateObjectList :: { [TPredObjList] }
    : PredObj                           { [$1] }
    | PredicateObjectList ';' PredObj   { $1 ++ [$3] }

PredObj :: { TPredObjList }
        : Predicate ObjectList   { ($1, $2) }

Predicate :: { TPred }
          : iriref   { TPIri $1 }
          | pname    { uncurry TPPName $1 }
          | 'a'      { TPA }

-- Left-recursive object list; ','-separated
ObjectList :: { [TObj] }
    : Obj                  { [$1] }
    | ObjectList ',' Obj   { $1 ++ [$3] }

Obj :: { TObj }
    : iriref   { TOIri $1 }
    | pname    { uncurry TOPName $1 }
    | strlit   { TOLit (LString $1) }
    | intlit   { TOLit (LInt    $1) }

{
parseError :: [Token] -> a
parseError ts =
  error ("Turtle parse error near: " ++ show (take 3 ts))
}
