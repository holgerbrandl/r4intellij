grammar RGrammar;

options {
  language = Java;
  output = AST;
  ASTLabelType='org.antlr.runtime.tree.CommonTree';
}


tokens {
  PROG;
  EXPR_OR_ASSIGN;
  EQUAL_ASSIGN;
  EXPR;
  EXPRLIST;
  COND;
  IFCOND;
  FORCOND;
  SUBLIST;
  SUB;
  FORMLIST;
  CR;
  COMMENT;
  MODIFIER;
  SYMBOL;
}


prog :
     '\n'
    | expr_or_assign '\n'
    | expr_or_assign ';'
    | COMMENT;


COMMENT	:
	'#' ;

expr_or_assign :
     expr | equal_assign ;


equal_assign :
     expr '=' expr_or_assign ;

expr :
     '{' exprlist '}'
    | '(' expr_or_assign ')'
    | '-' expr
    | '+' expr
    | '!' expr
    | '~' expr
    | '?' expr
    | expr ( ':' | '+' | '-' | '*' | '/' | '^' | '%' | '~' | '?' ) expr
    | 'function' '(' formlist ')' expr_or_assign
    | expr '(' sublist ')'
    | 'if' ifcond expr_or_assign
    | 'if' ifcond expr_or_assign 'else' expr_or_assign
    | 'for' forcond expr_or_assign
    | 'while' cond expr_or_assign
    | 'repeat' expr_or_assign
    | expr '[[' sublist ']]'
    | expr '[' sublist ']'
    | expr '$' SYMBOL
    | expr '$' STR_CONST
    | expr '@' SYMBOL
    | expr '@' STR_CONST
     | NUM_CONST
        | STR_CONST
        | NULL_CONST
        | SYMBOL
    | 'next'
    | 'break' ;

cond :
     '(' expr ')' ;

ifcond :
     '(' expr ')' ;

forcond :
     '(' SYMBOL 'in' expr ')' ;

exprlist :
     whitespace | expr_or_assign
        | exprlist ';' expr_or_assign
        | exprlist ';'
        | exprlist '\n' expr_or_assign
        | exprlist '\n' ;

sublist :
     sub | sublist cr ',' sub ;

sub :
     whitespace | expr
   | SYMBOL '='
   | SYMBOL '=' expr
   | STR_CONST '='
   | STR_CONST '=' expr
   | NULL_CONST '='
   | NULL_CONST '=' expr ;

formlist :
     whitespace | SYMBOL
        | SYMBOL '=' expr
        | formlist ',' SYMBOL
        | formlist ',' SYMBOL '=' expr
        | '...' ;

SYMBOL	:
	;

whitespace: ' '
	;

STR_CONST
	: 'A'	;

NULL_CONST
	: 'NULL'
	;

NUM_CONST
	: '45'
	;



cr : ;
     /* empty */ //todo
 
