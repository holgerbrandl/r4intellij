/** Based on the arc lexer (http://code.google.com/p/intelli-arc/) **/

package com.r4intellij.lang.lexer;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import static com.r4intellij.lang.lexer.RTokenTypes.*;

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
LineTerminator = \r|\n|\r\n
WhiteSpace= {LineTerminator} | [ \t\f]
Comment = "#"[^\r\n]*

/* A identifier integer is a word beginning a letter between A and Z, a and z,
or an underscore followed by zero or more letters between A and Z, a and z,
zero and nine, or an underscore. */
Identifier = [A-Za-z_][A-Za-z_0-9._]*


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
StringLiteral=\"([^\\\"]|{EscapeSequence})*(\"|\\)?

//%state STRING

%%

/* ------------------------Lexical Rules Section---------------------- */

/*
This section contains regular expressions and actions, i.e. Java code, that
will be executed when the scanner matches the associated regular expression. */

/* YYINITIAL is the state at which the lexer begins scanning.  So these regular
expressions will only be matched if the scanner is in the start state
YYINITIAL. */

<YYINITIAL> {

      /* If an identifier is found print it out , return the token ID that
    represents an identifier and the default value one that is given to all
    identifiers. */
    //{Variable} { System.out.print("word:"+yytext()); return WORD;}

    /* A list of prefdefined keywords in R. */
    "print"                      |
    "function"                      |
    "source"                     |
    "data.frame"                    |
    "times"                      |
    "trap"                       |
    "typeof"                       |
    "class"                       |
    "ulimit"                     |
    "umask"                      |
    "unalias"                    |
    "wait"                       { return INTERNAL_COMMAND; }


    "FALSE" | "F" | "TRUE" | "T" | "pi" | "NULL" { return CONSTANT; }


    // separators
    "(" { return LEFT_PAREN; }
    ")" { return RIGHT_PAREN; }
    "{" { return LEFT_CURLY; }
    "}" { return RIGHT_CURLY; }
    "[" { return LEFT_SQUARE; }
    "]" { return RIGHT_SQUARE; }
    ";" { return SEMICOLON; }
    "," { return COMMA; }


    // operators
    "-" { return ARITH_MINUS; }
    "+" { return ARITH_PLUS; }
    "!" { return NEGATION; }
    "~" { return TILDE; }
    "?" {}
    ":" { return COLON; }
    "*" { return ARITH_MULT; }
    "/" { return ARITH_DIV; }
    "^" { return ARITH_EXPONENTIAION; }
    "%%" { return ARITH_MOD; }

    "%/%" | "%*%" | "%o%" | "%x%" | "%in%"  { return ARITH_MISC; }

    "&" | "&&" | "|" | "||" | ("<"|">")[=]? { return LOG_OPERATOR; }

    "$" { return LIST_SUBSET; }

    // misc
    "..." { return VARARGS; }
    "<-" | "=" | "->"  { return ASSIGNMENT; }


    // todo refactor these away
    "." { return DOT; }

    // string literal
//    \" | \' {// yybegin(STRING); string.setLength(0); }

    {IntLiteral} | {DoubleLiteral}  { return NUMBER; }

    {Identifier} { return IDENTIFIER; }
//    {Identifier} {System.out.print("word:"+yytext());  return IDENTIFIER; }

    {Comment} {return COMMENT; }
    {WhiteSpace} { return com.intellij.psi.TokenType.WHITE_SPACE; }
//    {WhiteSpace} { /* skip this */ }

    {StringLiteral}  { return STRING_LITERAL; }
//    "\"" ({StringCharacter} | "\'" | "}")* "\""   { return STRING_LITERAL; }
//    "\'" ({StringCharacter} | "\"")* "\'"   { return STRING_LITERAL; }

   // "\"\\t\"" {return  STRING_LITERAL; }
}

//// note: picked up from java.flex
//<STRING> {
//  \"                             { yybegin(YYINITIAL); return STRING_LITERAL; /*, string.toString()); */ }
//  {StringCharacter}+             { string.append( yytext() ); }
//
//  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
//  {LineTerminator}               { throw new RuntimeException("Unterminated string at end of line"); }
//}



/* No token was found for the input so through an error.  Print out an
'Illegal character' message with the illegal character that was found.*/
//. { throw new Error("Illegal character <"+yytext()+">");}
.               { return BAD_CHARACTER; }