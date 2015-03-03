/** Based on the arc lexer (http://code.google.com/p/intelli-arc/) **/

package com.r4intellij.lang.lexer;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import static com.r4intellij.psi.RTypes.*;


import com.intellij.util.containers.Stack;

%%


%class _RLexer
%implements FlexLexer
%unicode
%public
%char

%function advance
%type IElementType

%{
  StringBuffer string = new StringBuffer();

  //helper
  long yychar = 0;
%}

/*
Macro Declarations

These declarations are regular expressions that will be used latter in the
Lexical Rules Section.
*/

/* A line terminator is a \r (carriage return), \n (line feed), or \r\n. */
EOL = (\r|\n|\r\n)*
//WHITE_SPACE= {EOL} | [ \t\f]
WHITE_SPACE= [ \t\f]
SECTION_COMMENT = [#]{5,120}[\r\n]{1} [#]{2,5} [^\r\n]*
COMMENT = "#"[^\r\n]*

/* A identifier integer is a word beginning a letter between A and Z, a and z,
or an underscore followed by zero or more letters between A and Z, a and z,
zero and nine, or an underscore. */
SYMBOL = [A-Za-z.][A-Za-z_0-9._]*


/* A literal integer is is a number beginning with a number between one and nine
followed by zero or more numbers between zero and nine or just a zero. */
IntLiteral = 0 | [1-9][0-9]*[L]?

Exponent = [eE] [+-]? [0-9]+
FLit1    = [0-9]+ \. [0-9]*
FLit2    = \. [0-9]+
FLit3    = [0-9]+
DoubleLiteral = ({FLit1}|{FLit2}|{FLit3}) {Exponent}?


//StringCharacter = [^\r\n]
// picked up from arc.flex :
EscapeSequence=\\[^\r\n]
//todo allow for linebreaks in strings and for single quot quoting
STRING_DQUOTE=\"([^\\\"]|{EscapeSequence})*(\"|\\)?
STRING_SQUOTE='([^\\\']|{EscapeSequence})*('|\\)?

//%state STRING

/* ------------------------Lexical Rules Section---------------------- */

/*
This section contains regular expressions and actions, i.e. Java code, that
will be executed when the scanner matches the associated regular expression. */

/* YYINITIAL is the state at which the lexer begins scanning.  So these regular
expressions will only be matched if the scanner is in the start state
YYINITIAL. */

%%

<YYINITIAL> {
  {WHITE_SPACE} {yybegin(YYINITIAL); return com.intellij.psi.TokenType.WHITE_SPACE; }
  {EOL} {yybegin(YYINITIAL); return R_EOL; }
  {SECTION_COMMENT} {yybegin(YYINITIAL); return R_SECTION_COMMENT; }
  {COMMENT} {yybegin(YYINITIAL); return R_COMMENT; }

  // r keywords
  "function" { return R_FUNCTION; }
  "for" { return R_FOR; }
  "while" { return R_WHILE; }
  "if" { return R_IF; }
  "else" { return R_ELSE; }
  "break" { return R_BREAK; }
  "next" { return R_NEXT; }
  "repeat" { return R_REPEAT; }
  "in" { return R_IN; }
  "NULL" { return R_NULL_CONST; }
  "..." { return R_SYMBOL_FORMALS; }

  {STRING_SQUOTE} | {STRING_DQUOTE} {yybegin(YYINITIAL); return RTypes.R_STR_CONST; }
 {SYMBOL} { yybegin(YYINITIAL); return RTypes.R_SYMBOL; }
 // {SYMBOL} {System.out.print("word:"+yytext()); yybegin(YYINITIAL); return RTypes.R_SYMBOL; }

  //{NUMBER} {yybegin(YYINITIAL); return RTypes.R_NUM_CONST; }
  {IntLiteral} | {DoubleLiteral}  { return R_NUM_CONST; }
  "NULL" { return R_NULL_CONST; }

    // separators
  ";" {yybegin(YYINITIAL); return RTypes.R_SEMICOLON; }
  ":" {yybegin(YYINITIAL); return R_COLON; }
  "," {yybegin(YYINITIAL); return R_COMMA; }
  "(" {yybegin(YYINITIAL); return RTypes.R_LEFT_PAREN; }
  ")" {yybegin(YYINITIAL); return RTypes.R_RIGHT_PAREN; }
  "{" {yybegin(YYINITIAL); return RTypes.R_LEFT_BRACE; }
  "}" {yybegin(YYINITIAL); return RTypes.R_RIGHT_BRACE; }
  "[" {yybegin(YYINITIAL); return RTypes.R_LEFT_BRACKET; }
  "]" {yybegin(YYINITIAL); return RTypes.R_RIGHT_BRACKET; }
  "[[" {yybegin(YYINITIAL); return RTypes.R_LBB; }
  "]]" {yybegin(YYINITIAL); return RTypes.R_RBB; }

  // logical operators
  // unary
  "!" { return R_NEGATION; }
  // binary
  "==" { return R_EQ; }
  ">" { return R_GT; }
  "<" { return R_LT; }
  ">=" { return R_GE; }
  "<=" { return R_LE; }
  "!=" { return R_NE; }
  "&" { return R_AND; } // not vectorized
  "&&" { return R_AND2; } // not vectorized
  "|" { return R_OR; }
  "||" { return R_OR2; }

  // operators
  "-" { return R_ARITH_MINUS; }
  "+" { return R_ARITH_PLUS; }
  "~" { return R_TILDE; }
  "*" { return R_ARITH_MULT; }
  "%" { return R_ARITH_MOD; }
  "/" { return R_ARITH_DIV; }
  "^" { return R_ARITH_EXPONENTIAION; }
  "%%" { return R_ARITH_MOD; }
  "%/%" | "%*%" | "%o%" | "%x%" | "%+%" | "%>%" | "%<>%" | "%$%" | "%T>%" | %in% { return R_ARITH_MISC; }


    // misc
    "=" { return R_EQ_ASSIGN; }
    "<-" { return R_LEFT_ASSIGN; }
    "->" { return R_RIGHT_ASSIGN; }
    "->>" { return R_GLOBAL_RIGHT_ASSIGN; }
    "<<-" { return R_GLOBAL_LEFT_ASSIGN; }

   "$" { return R_LIST_SUBSET; }
   "@" { return R_SLOT; }
    "?" { return R_QUESTION; }
    "::" {yybegin(YYINITIAL); return RTypes.R_NS_GET; }
    ":::" {yybegin(YYINITIAL); return RTypes.R_NS_GET_INT; }
}

.    { return com.intellij.psi.TokenType.BAD_CHARACTER; }
//<<EOF>>  { return RTypes.R_EOF; }
