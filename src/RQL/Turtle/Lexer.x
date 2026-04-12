{
{-# OPTIONS_GHC -w #-}
module RQL.Turtle.Lexer (Token (..), alexScanTokens) where
}

%wrapper "basic"

-- ---------------------------------------------------------------------------
-- Character classes
-- ---------------------------------------------------------------------------

$digit   = [0-9]
$alpha   = [a-zA-Z]

-- Characters allowed inside PN_PREFIX / PN_LOCAL (simplified subset)
$pnchar  = [a-zA-Z0-9_]

-- IRI reference content: anything except whitespace, \60=< \62=> and \\
-- (Using decimal escapes because Alex treats bare < > as start-code syntax)
$irichar = [^ \t\n\r\60\62\\]

-- Double-quote character (\34 = 0x22 = ") as a macro so Alex builds the
-- DFA transition correctly (bare " in rule strings is mishandled by Alex).
$dquote  = [\34]

-- String literal content: anything except double-quote, newline, backslash
$strchar = [^\34\n\\]

-- ---------------------------------------------------------------------------
-- Token rules  (Alex uses maximal munch; earlier rules win on equal length)
-- ---------------------------------------------------------------------------

tokens :-

  -- Whitespace (including \r from Windows CRLF) and line comments: skip silently
  [\ \t\n\r\f]+   ;
  "#" [^\n]*      ;

  -- Keywords (must precede any @-prefixed catch-all)
  "@base"         { \_ -> TokBase }
  "@prefix"       { \_ -> TokPrefix }

  -- IRI reference:  < ... >
  "<" $irichar* ">"        { \s -> TokIriRef (tail (init s)) }

  -- String literal (plain, no escape sequences):  " ... "
  -- Use $dquote macro ([\34]) so Alex builds the DFA transition for byte 34.
  $dquote $strchar* $dquote  { \s -> TokStringLit (tail (init s)) }

  -- Integer literal
  $digit+                  { \s -> TokIntLit (read s) }

  -- The 'a' keyword (rdf:type shorthand).
  -- Maximal munch ensures "a:foo" is taken by the PNameLN rules below.
  "a"                      { \_ -> TokA }

  -- Prefixed name with non-empty local part:  prefix:local
  $alpha $pnchar* ":" $pnchar+
    { \s -> let (pre, _:loc) = break (== ':') s
            in  TokPNameLN (pre, loc) }

  -- Namespace-only token used in @prefix declarations:  ns:
  $alpha $pnchar* ":"
    { \s -> TokPNameLN (init s, "") }

  -- Punctuation
  "."             { \_ -> TokDot }
  ";"             { \_ -> TokSemicolon }
  ","             { \_ -> TokComma }

{
-- | Tokens produced by the Turtle lexer.
data Token
  = TokBase
  | TokPrefix
  | TokA
  | TokIriRef    String
  | TokPNameLN   (String, String)   -- ^ (prefix-namespace, local-name)
  | TokStringLit String
  | TokIntLit    Int
  | TokDot
  | TokSemicolon
  | TokComma
  deriving (Eq, Show)
}
