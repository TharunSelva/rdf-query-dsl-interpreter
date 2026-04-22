{
module RQL.Syntax.Parser
  ( parseProgram
  , parseProgramFile
  ) where

-- import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Exception (IOException, SomeException, displayException, evaluate, try)
import RQL.Common.Errors (ParseError (..))
import RQL.Common.Types
import RQL.Syntax.AST
import RQL.Syntax.Lexer (Token (..), alexScanTokens)
}

%name      parseProgram
%tokentype { Token }
%error     { parseError }

%token
  let          { TokLet }
  '='          { TokEquals }
  load         { TokLoad }
  print        { TokPrint }
  union        { TokUnion }
  intersection { TokIntersection }
  difference   { TokDifference }
  match        { TokMatch }
  filter       { TokFilter }
  where        { TokWhere }
  construct    { TokConstruct }
  from         { TokFrom }
  in           { TokIn }
  groupmax     { TokGroupMax }
  groupmin     { TokGroupMin }
  chain        { TokChain }
  typeInfer    { TokTypeInfer }
  var          { TokVar $$ }
  iriref       { TokIriRef $$ }
  strlit       { TokStringLit $$ }
  intlit       { TokIntLit $$ }
  ident        { TokIdent $$ }
  '{'          { TokLBrace }
  '}'          { TokRBrace }
  '('          { TokLParen }
  ')'          { TokRParen }
  ','          { TokComma }
  '>='         { TokGe }
  '<='         { TokLe }
  '<'          { TokLt }
  '>'          { TokGt }
  '!='         { TokNe }
  '!'          { TokNot }
  '=='         { TokEqEq }
  '&&'         { TokAnd }
  '||'         { TokOr }

%%

Program :: { Program }
  : RevStmts                      { Program (reverse $1) }

RevStmts :: { [Stmt] }
  : {- empty -}                   { [] }
  | RevStmts Stmt                 { $2 : $1 }

Stmt :: { Stmt }
  : let ident '=' Expr            { SLet $2 $4 }
  | print Expr                    { SPrint $2 }

Expr :: { Expr }
  : ident                         { EVar $1 }
  | load strlit                   { ELoad $2 }
  | union Expr Expr               { EUnion $2 $3 }
  | intersection Expr Expr        { EIntersection $2 $3 }
  | difference Expr Expr          { EDifference $2 $3 }
  | match '{' RevMatchClauses '}' { EMatch (reverse $3) }
  | filter Expr where '(' Pred ')' { EFilter $5 $2 }
  | construct '{' RevTPatterns '}' from Expr
                                  { EConstruct (reverse $3) $6 }
  | groupmax Expr var iriref      { EGroupMax $2 $3 (TIri $4) }
  | groupmin Expr var iriref      { EGroupMin $2 $3 $4 }
  | chain Expr Expr               { EChain $2 $3 }
  | typeInfer '(' Expr ',' Expr ')' { ETypeInfer $3 $5 }

RevMatchClauses :: { [GraphPattern] }
  : MatchClause                   { [$1] }
  | RevMatchClauses MatchClause   { $2 : $1 }

MatchClause :: { GraphPattern }
  : in ident '{' PatTerm PatTerm PatTerm '}'
                                  { GraphPattern $2 (TriplePattern $4 $5 $6) }

RevTPatterns :: { [TriplePattern] }
  : TPattern                      { [$1] }
  | RevTPatterns TPattern         { $2 : $1 }

TPattern :: { TriplePattern }
  : PatTerm PatTerm PatTerm       { TriplePattern $1 $2 $3 }

PatTerm :: { PatternTerm }
  : var                           { PVar $1 }
  | iriref                        { PConst (TIri $1) }
  | strlit                        { PConst (TLit (LString $1)) }
  | intlit                        { PConst (TLit (LInt $1)) }

Pred :: { Predicate }
  : OrPred                        { $1 }

OrPred :: { Predicate }
  : OrPred '||' AndPred           { POr $1 $3 }
  | AndPred                       { $1 }

AndPred :: { Predicate }
  : AndPred '&&' NotPred          { PAnd $1 $3 }
  | NotPred                       { $1 }

NotPred :: { Predicate }
  : '!' NotPred                   { PNot $2 }
  | AtomPred                      { $1 }

AtomPred :: { Predicate }
  : PatTerm '>=' PatTerm          { PGe $1 $3 }
  | PatTerm '<=' PatTerm          { PLe $1 $3 }
  | PatTerm '<' PatTerm           { PLt $1 $3 }
  | PatTerm '>' PatTerm           { PGt $1 $3 }
  | PatTerm '!=' PatTerm          { PNe $1 $3 }
  | PatTerm '==' PatTerm          { PEq $1 $3 }
  | '(' Pred ')'                  { $2 }


{
-- previous parse error:
-- parseError :: [Token] -> a
-- parseError _ = error "Program parse error"
parseError :: [Token] -> a
parseError [] =
  error "RQL parse error: unexpected end of input"
parseError ts =
  error ("RQL parse error near: " ++ show (take 3 ts))


-- prev version:
-- parseProgramFile :: FilePath -> IO (Either ParseError Program)
-- parseProgramFile path = do
--   contents <- readFile path
--   let tokens = alexScanTokens contents
--   pure (Right (parseProgram tokens))


parseProgramFile :: FilePath -> IO (Either ParseError Program)
parseProgramFile path = do
  contentsResult <- try (readProgramContents path) :: IO (Either IOException String)
  case contentsResult of
    Left ex ->
      pure (Left (ParseError ("could not read program file '" ++ path ++ "': " ++ displayException ex)))
    Right contents -> do
      result <- (try $ do
        let tokens = alexScanTokens contents
        evaluate (parseProgram tokens)
        ) :: IO (Either SomeException Program)

      case result of
        Left ex ->
          pure (Left (ParseError ("invalid RQL program: " ++ displayException ex)))
        Right program ->
          pure (Right program)

readProgramContents :: FilePath -> IO String
readProgramContents path = do
  contents <- readFile path
  _ <- evaluate (length contents)
  pure contents
}