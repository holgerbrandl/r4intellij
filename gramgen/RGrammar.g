grammar RGrammar;

options {
  language = Java;
  output = AST;
  ASTLabelType='org.antlr.runtime.tree.CommonTree';
}


tokens {
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
     ('{' exprlist '}' | '(' expr_or_assign ')' | '-' expr | '+' expr | '!' expr | '~' expr | '?' expr | 'function' '(' formlist ')' expr_or_assign | 'if' ifcond expr_or_assign | 'if' ifcond expr_or_assign 'else' expr_or_assign | 'for' forcond expr_or_assign | 'while' cond expr_or_assign | 'repeat' expr_or_assign | NUM_CONST | STR_CONST | NULL_CONST | SYMBOL | 'next' | 'break') (( ':' | '+' | '-' | '*' | '/' | '^' | '%' | '~' | '?' ) expr | '(' sublist ')' | '[[' sublist ']]' | '[' sublist ']' | '$' SYMBOL | '$' STR_CONST | '@' SYMBOL | '@' STR_CONST)* ;

cond :
     '(' expr ')' ;

ifcond :
     '(' expr ')' ;

forcond :
     '(' SYMBOL 'in' expr ')' ;

exprlist :
     (whitespace | expr_or_assign) (';' expr_or_assign | ';' | '\n' expr_or_assign | '\n')* ;

sublist :
     (sub) (cr ',' sub)* ;

sub :
     whitespace | expr
   | SYMBOL '='
   | SYMBOL '=' expr
   | STR_CONST '='
   | STR_CONST '=' expr
   | NULL_CONST '='
   | NULL_CONST '=' expr ;

formlist :
     (whitespace | SYMBOL | SYMBOL '=' expr | '...') (',' SYMBOL | ',' SYMBOL '=' expr)* ;

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
 
