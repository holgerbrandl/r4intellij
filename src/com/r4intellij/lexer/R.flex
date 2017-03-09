/* It's an automatically generated code. Do not modify it. */
package com.r4intellij.lexer;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;
import java.util.Stack;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.parsing.RParserDefinition;

%%

%class _RLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{  return;
%eof}

WHITE_SPACE_CHAR=[\ \n\r\t\f]

// identifiers
// Identifiers consist of a sequence of letters, digits, the period (‘.’) and the underscore.
// They must not start with a digit or an underscore, or with a period followed by a digit.
// TODO: Notice also that objects can have names that are not identifiers! ("x" <- 1, assign("x$a",1))
LETTER = [a-zA-Z]|[:unicode_uppercase_letter:]|[:unicode_lowercase_letter:]|[:unicode_titlecase_letter:]|[:unicode_modifier_letter:]|[:unicode_other_letter:]|[:unicode_letter_number:]
IDENT_START = {LETTER}|"."{LETTER}|"._"|".."
IDENT_CONTINUE = {LETTER}|[0-9_"."]
QUOTED_IDENTIFIER = "`" [^`]* "`"
IDENTIFIER = {IDENT_START}{IDENT_CONTINUE}** | {QUOTED_IDENTIFIER} | "."

LETTER_OR_OP = {LETTER} | "+"|"-"|"*"|"&"|"^"|"$"|"/"|"~"|">"|"<"|"="|"."|"|"|":"
END_OF_LINE_COMMENT="#"[^\r\n]*


// numeric constants
DIGIT = [0-9]
NONZERO_DIGIT = [1-9]
HEX_DIGIT = [0-9A-Fa-f]
OCT_DIGIT = [0-7]
NONZERO_OCT_DIGIT = [1-7]
HEX_INTEGER = 0[Xx]({HEX_DIGIT})+
DECIMAL_INTEGER = ({DIGIT}({DIGIT})*)
INTEGER = {DECIMAL_INTEGER}|{HEX_INTEGER}                                  // essential

INT_PART = ({DIGIT})+
FRACTION = \.({DIGIT})+
EXPONENT = [eE][+\-]?({DIGIT})+
BINARY_EXPONENT = [pP][+\-]?({DIGIT})+
POINT_FLOAT=(({INT_PART})?{FRACTION})|({INT_PART}\.)
EXPONENT_HEX = ({HEX_INTEGER}|({HEX_INTEGER}{FRACTION})){BINARY_EXPONENT}
EXPONENT_FLOAT=(({INT_PART})|({POINT_FLOAT})){EXPONENT}
FLOAT_NUMBER=({POINT_FLOAT})|({EXPONENT_FLOAT})|({EXPONENT_HEX})             // essential

// integer constants
LONG_INTEGER = ({INTEGER} | {FLOAT_NUMBER})[Ll]                                              // essential

// complex constants
COMPLEX_NUMBER=(({FLOAT_NUMBER})|({INT_PART}))[i]             // essential

// string constants
QUOTED_LITERAL="'"([^\\\']|{ANY_ESCAPE_SEQUENCE})*?("'")?
DOUBLE_QUOTED_LITERAL=\"([^\\\"]|{ANY_ESCAPE_SEQUENCE})*?(\")?
ANY_ESCAPE_SEQUENCE = \\[^]
STRING=({QUOTED_LITERAL} | {DOUBLE_QUOTED_LITERAL})
//ESCAPE_SEQUENCE=\\([rntbafv\'\"\\]|{NONZERO_OCT_DIGIT}|{OCT_DIGIT}{2,3}|"x"{HEX_DIGIT}{1,2}|"u"{HEX_DIGIT}{1,4}|"u{"{HEX_DIGIT}{1,4}"}"|"U"{HEX_DIGIT}{1,8}|"U{"{HEX_DIGIT}{1,8}"}")

%{
private Stack<IElementType> myExpectedBracketsStack = new Stack<IElementType>();
%}

%%

<YYINITIAL> {
[\n]                        { return RElementTypes.R_NL; }
{END_OF_LINE_COMMENT}       { return RParserDefinition.END_OF_LINE_COMMENT; }
[\ ]                        { return RParserDefinition.SPACE; }
[\t]                        { return RParserDefinition.TAB; }
[\f]                        { return RParserDefinition.FORMFEED; }

// logical constants
"TRUE"                      { return RElementTypes.R_TRUE; }
"FALSE"                     { return RElementTypes.R_FALSE; }
"T"                         { return RElementTypes.R_TRUE; }
"F"                         { return RElementTypes.R_FALSE; }

// numeric constants
{INTEGER}                   { return RElementTypes.R_NUMERIC; }
{FLOAT_NUMBER}              { return RElementTypes.R_NUMERIC; }

// complex constants
{COMPLEX_NUMBER}            { return RElementTypes.R_COMPLEX; }

// integer constants
{LONG_INTEGER}              { return RElementTypes.R_INTEGER; }

// string constants
{STRING}
                  { return RElementTypes.R_STRING; }
// special constants
"NULL"                      { return RElementTypes.R_NULL; }
"NA"                        { return RElementTypes.R_NA; }
"Inf"                       { return RElementTypes.R_INF; }
"NaN"                       { return RElementTypes.R_NAN; }

"NA_integer_"               { return RElementTypes.R_NA_INTEGER; }
"NA_real_"                  { return RElementTypes.R_NA_REAL; }
"NA_complex_"               { return RElementTypes.R_NA_COMPLEX; }
"NA_character_"             { return RElementTypes.R_NA_CHARACTER; }

"if"                        { return RElementTypes.R_IF; }
"else"                      { return RElementTypes.R_ELSE; }
"repeat"                    { return RElementTypes.R_REPEAT; }
"while"                     { return RElementTypes.R_WHILE; }
"function"                  { return RElementTypes.R_FUNCTION; }
"for"                       { return RElementTypes.R_FOR; }
"in"                        { return RElementTypes.R_IN; }
"next"                      { return RElementTypes.R_NEXT; }
"break"                     { return RElementTypes.R_BREAK; }
"..."                       { return RElementTypes.R_TRIPLE_DOTS; }

{IDENTIFIER}                { return RElementTypes.R_IDENTIFIER; }

//special operators
"%"{LETTER_OR_OP}*"%"       { return RElementTypes.R_INFIX_OP; }

// Infix and prefix operators
":::"                       { return RElementTypes.R_TRIPLECOLON; }
"::"                        { return RElementTypes.R_DOUBLECOLON; }
"@"                         { return RElementTypes.R_AT; }
"&&"                        { return RElementTypes.R_ANDAND; }
"||"                        { return RElementTypes.R_OROR; }


//arithmetic
"-"                         { return RElementTypes.R_MINUS; }
"+"                         { return RElementTypes.R_PLUS; }
"*"                         { return RElementTypes.R_MULT; }
"/"                         { return RElementTypes.R_DIV; }
"^"                         { return RElementTypes.R_EXP; }

// relational
"<"                         { return RElementTypes.R_LT; }
">"                         { return RElementTypes.R_GT; }
"=="                        { return RElementTypes.R_EQEQ; }
">="                        { return RElementTypes.R_GE; }
"<="                        { return RElementTypes.R_LE; }
"!="                        { return RElementTypes.R_NOTEQ; }

// logical
"!"                         { return RElementTypes.R_NOT; }
"|"                         { return RElementTypes.R_OR; }
"&"                         { return RElementTypes.R_AND; }

// model formulae
"~"                         { return RElementTypes.R_TILDE; }

// assign
"<<-"                       { return RElementTypes.R_LEFT_COMPLEX_ASSIGN; }
"->>"                       { return RElementTypes.R_RIGHT_COMPLEX_ASSIGN; }
"<-"                        { return RElementTypes.R_LEFT_ASSIGN; }
"->"                        { return RElementTypes.R_RIGHT_ASSIGN; }
"="                         { return RElementTypes.R_EQ; }

// list indexing
"$"                         { return RElementTypes.R_LIST_SUBSET; }

// sequence
":"                         { return RElementTypes.R_COLON; }

// grouping
"("                         { return RElementTypes.R_LPAR; }
")"                         { return RElementTypes.R_RPAR; }
"{"                         { return RElementTypes.R_LBRACE; }
"}"                         { return RElementTypes.R_RBRACE; }

// indexing
"[["                        { myExpectedBracketsStack.add(RElementTypes.R_RDBRACKET); return RElementTypes.R_LDBRACKET; }
"]]"                        {
                              if (myExpectedBracketsStack.isEmpty()) return RElementTypes.R_RDBRACKET;
                              final IElementType expectedBracket = myExpectedBracketsStack.pop();
                              if (expectedBracket == RElementTypes.R_RDBRACKET) {
                                return RElementTypes.R_RDBRACKET;
                              }
                              else {
                                yypushback(1);
                                return RElementTypes.R_RBRACKET;
                              }
                              }
"["                         { myExpectedBracketsStack.add(RElementTypes.R_RBRACKET); return RElementTypes.R_LBRACKET; }
"]"                         {
                              if (myExpectedBracketsStack.isEmpty()) return RElementTypes.R_RBRACKET;
                              myExpectedBracketsStack.pop();
                              return RElementTypes.R_RBRACKET; }

// separators
","                         { return RElementTypes.R_COMMA; }
";"                         { return RElementTypes.R_SEMI; }

"?"                         { return RElementTypes.R_HELP; }
.                           { return RParserDefinition.BAD_CHARACTER; }

}