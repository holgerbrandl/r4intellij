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
%eof{   return;
%eof}


%{

  StringBuffer string = new StringBuffer();

  //helper
  long yychar = 0;

  public void goTo(int offset) {
    zzCurrentPos = zzMarkedPos = zzStartRead = offset;
    zzPushbackPos = 0;
    zzAtEOF = offset < zzEndRead;
  }
%}

/*
Macro Declarations

These declarations are regular expressions that will be used latter in the
Lexical Rules Section.
*/

/* A line terminator is a \r (carriage return), \n (line feed), or \r\n. */
EOL = [\r\n]
//EOL = ("\r"|"\n"|"\r\n")*
//EOF = <<eof>>
//LINE_WS=[ \t\f]
//WHITE_SPACE= {LINE_WS}
//WHITE_SPACE=({LINE_WS}|{EOL})+

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



WHITE_SPACE_CHAR=[\ \t\f]
//IDENTIFIER=[:jletter:] [:jletterdigit:]*
//
//DIGIT = [0-9]
//DIGIT_OR_UNDERSCORE = [_0-9]
//DIGITS = {DIGIT} | {DIGIT} {DIGIT_OR_UNDERSCORE}*
//HEX_DIGIT_OR_UNDERSCORE = [_0-9A-Fa-f]
//
//INTEGER_LITERAL = {DIGITS}


//%state STRING

/* ------------------------Lexical Rules Section---------------------- */

/*
This section contains regular expressions and actions, i.e. Java code, that
will be executed when the scanner matches the associated regular expression. */

/* YYINITIAL is the state at which the lexer begins scanning.  So these regular
expressions will only be matched if the scanner is in the start state
YYINITIAL. */

%state FUNCTION_CALL

%%
//<YYINITIAL>  <<EOF>>    { return R_EOF; }

<YYINITIAL> {
  {WHITE_SPACE_CHAR}+ { return com.intellij.psi.TokenType.WHITE_SPACE; }
  {EOL}* { return R_EOL; }

  {SECTION_COMMENT} { return R_SECTION_COMMENT; }
  {COMMENT} { return R_COMMENT; }

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

  // separators
  ";" { return R_SEMICOLON; }
  ":" { return R_COLON; }
  "," { return R_COMMA; }
  "(" { return R_LEFT_PAREN; }
  ")" { return R_RIGHT_PAREN; }
  "{" { return R_LEFT_BRACE; }
  "}" { return R_RIGHT_BRACE; }
  "[" { return R_LEFT_BRACKET; }
  "]" { return R_RIGHT_BRACKET; }
  "[[" { return R_LBB; }
  "]]" { return R_RBB; }

  {STRING_SQUOTE} | {STRING_DQUOTE} { return R_STR_CONST; }
  {IntLiteral} | {DoubleLiteral}  { return R_NUM_CONST; }
  {SYMBOL} 				{ return R_SYMBOL; }

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
  "%/%" | "%*%" | "%o%" | "%x%" | "%+%" | "%>%" | %in% { return R_ARITH_MISC; }


  // misc
  "=" { return R_EQ_ASSIGN; }
  "<-" { return R_LEFT_ASSIGN; }
  "->" { return R_RIGHT_ASSIGN; }
  "->>" { return R_GLOBAL_RIGHT_ASSIGN; }
  "<<-" { return R_GLOBAL_LEFT_ASSIGN; }

  "$" { return R_LIST_SUBSET; }
  "@" { return R_SLOT; }
  "?" { return R_QUESTION; }
  "::" { return R_NS_GET; }
  ":::" { return R_NS_GET_INT; }

  .    { return com.intellij.psi.TokenType.BAD_CHARACTER; }
}
