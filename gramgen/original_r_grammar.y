%}
/*{{{ Grammar */
/*{{{ Tokens */
%token		END_OF_INPUT ERROR
%token		STR_CONST NUM_CONST NULL_CONST SYMBOL FUNCTION
%token		LEFT_ASSIGN EQ_ASSIGN RIGHT_ASSIGN LBB
%token		FOR IN IF ELSE WHILE NEXT BREAK REPEAT
%token		GT GE LT LE EQ NE AND OR AND2 OR2
%token		NS_GET NS_GET_INT
%token		COMMENT SPACES ROXYGEN_COMMENT
%token		SYMBOL_FORMALS
%token		EQ_FORMALS
%token		EQ_SUB SYMBOL_SUB
%token		SYMBOL_FUNCTION_CALL
%token		SYMBOL_PACKAGE
%token		COLON_ASSIGN
%token		SLOT
/*}}}*/

/*{{{ This is the precedence table, low to high */
%left		'?'
%left		LOW WHILE FOR REPEAT
%right		IF
%left		ELSE
%right		LEFT_ASSIGN
%right		EQ_ASSIGN
%left		RIGHT_ASSIGN
%left		'~' TILDE
%left		OR OR2
%left		AND AND2
%left		UNOT NOT
%nonassoc   	GT GE LT LE EQ NE
%left		'+' '-'
%left		'*' '/'
%left		SPECIAL
%left		':'
%left		UMINUS UPLUS
%right		'^'
%left		'$' '@'
%left		NS_GET NS_GET_INT
%nonassoc	'(' '[' LBB
/*}}}*/

/*{{{ rules */
%%

prog	:	END_OF_INPUT			{ return 0; }
	|	'\n'						{ return xxvalue(NULL,2); }
	|	expr_or_assign '\n'			{ return xxvalue($1,3); }
	|	expr_or_assign ';'			{ return xxvalue($1,4); }
	|	error	 					{ YYABORT; }
	;

expr_or_assign  :    expr         { $$ = $1; }
                |    equal_assign { $$ = $1; }
                ;

equal_assign    :    expr EQ_ASSIGN expr_or_assign  { $$ = xxbinary($2,$1,$3); setId( $$, @$ ) ; }
                ;

expr	: 	NUM_CONST					{ $$ = $1;  setId( $$, @$); }
	|	STR_CONST						{ $$ = $1;  setId( $$, @$); }
	|	NULL_CONST						{ $$ = $1;  setId( $$, @$); }
	|	SYMBOL							{ $$ = $1;  setId( $$, @$); }

	|	'{' exprlist '}'				{ $$ = xxexprlist($1,$2);  setId( $$, @$); }
	|	'(' expr_or_assign ')'			{ $$ = xxparen($1,$2);		setId( $$, @$); }

	|	'-' expr %prec UMINUS			{ $$ = xxunary($1,$2);     setId( $$, @$); }
	|	'+' expr %prec UMINUS			{ $$ = xxunary($1,$2);     setId( $$, @$); }
	|	'!' expr %prec UNOT				{ $$ = xxunary($1,$2);     setId( $$, @$); }
	|	'~' expr %prec TILDE			{ $$ = xxunary($1,$2);     setId( $$, @$); }
	|	'?' expr						{ $$ = xxunary($1,$2);     setId( $$, @$); }

	|	expr ':'  expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '+'  expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '-' expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '*' expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '/' expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '^' expr 					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr SPECIAL expr				{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '%' expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '~' expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr '?' expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr LT expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr LE expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr EQ expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr NE expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr GE expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr GT expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr AND expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr OR expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr AND2 expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }
	|	expr OR2 expr					{ $$ = xxbinary($2,$1,$3);     setId( $$, @$); }

	|	expr LEFT_ASSIGN expr 			{ $$ = xxbinary($2,$1,$3); setId( $$, @$); }
	|	expr RIGHT_ASSIGN expr 		{ $$ = xxbinary($2,$3,$1); setId( $$, @$); }
	|	FUNCTION '(' formlist ')' cr expr_or_assign %prec LOW
										{ $$ = xxdefun($1,$3,$6);             setId( $$, @$); }

	|	expr '(' sublist ')'			{ $$ = xxfuncall($1,$3);  setId( $$, @$); modif_token( &@1, SYMBOL_FUNCTION_CALL ) ; }
	|	IF ifcond expr_or_assign 		{ $$ = xxif($1,$2,$3);    setId( $$, @$); }
	|	IF ifcond expr_or_assign ELSE expr_or_assign
										{ $$ = xxifelse($1,$2,$3,$5); setId( $$, @$); }

	|	FOR forcond expr_or_assign %prec FOR
										{ $$ = xxfor($1,$2,$3); setId( $$, @$); }
	|	WHILE cond expr_or_assign		{ $$ = xxwhile($1,$2,$3);   setId( $$, @$); }
	|	REPEAT expr_or_assign			{ $$ = xxrepeat($1,$2);         setId( $$, @$);}
	|	expr LBB sublist ']' ']'		{ $$ = xxsubscript($1,$2,$3);       setId( $$, @$); }
	|	expr '[' sublist ']'			{ $$ = xxsubscript($1,$2,$3);       setId( $$, @$); }
	|	SYMBOL NS_GET SYMBOL			{ $$ = xxbinary($2,$1,$3);      	 setId( $$, @$); modif_token( &@1, SYMBOL_PACKAGE ) ; }
	|	SYMBOL NS_GET STR_CONST		{ $$ = xxbinary($2,$1,$3);      setId( $$, @$);modif_token( &@1, SYMBOL_PACKAGE ) ; }
	|	STR_CONST NS_GET SYMBOL		{ $$ = xxbinary($2,$1,$3);      setId( $$, @$); }
	|	STR_CONST NS_GET STR_CONST		{ $$ = xxbinary($2,$1,$3);          setId( $$, @$); }
	|	SYMBOL NS_GET_INT SYMBOL		{ $$ = xxbinary($2,$1,$3);          setId( $$, @$); modif_token( &@1, SYMBOL_PACKAGE ) ;}
	|	SYMBOL NS_GET_INT STR_CONST	{ $$ = xxbinary($2,$1,$3);      setId( $$, @$); modif_token( &@1, SYMBOL_PACKAGE ) ;}
	|	STR_CONST NS_GET_INT SYMBOL	{ $$ = xxbinary($2,$1,$3);      setId( $$, @$); }
	|	STR_CONST NS_GET_INT STR_CONST	{ $$ = xxbinary($2,$1,$3 );     setId( $$, @$);}
	|	expr '$' SYMBOL					{ $$ = xxbinary($2,$1,$3);              setId( $$, @$); }
	|	expr '$' STR_CONST				{ $$ = xxbinary($2,$1,$3);              setId( $$, @$); }
	|	expr '@' SYMBOL					{ $$ = xxbinary($2,$1,$3);              setId( $$, @$); modif_token( &@3, SLOT ) ; }
	|	expr '@' STR_CONST				{ $$ = xxbinary($2,$1,$3);              setId( $$, @$); }
	|	NEXT							{ $$ = xxnxtbrk($1);                       setId( $$, @$); }
	|	BREAK							{ $$ = xxnxtbrk($1);                       setId( $$, @$); }
	;


cond	:	'(' expr ')'				{ $$ = xxcond($2); }
	;

ifcond	:	'(' expr ')'				{ $$ = xxifcond($2); }
	;

forcond :	'(' SYMBOL IN expr ')' 	{ $$ = xxforcond($2,$4); }
	;


exprlist:								{ $$ = xxexprlist0(); }
	|	expr_or_assign					{ $$ = xxexprlist1($1); }
	|	exprlist ';' expr_or_assign	{ $$ = xxexprlist2($1, $3); }
	|	exprlist ';'					{ $$ = $1; }
	|	exprlist '\n' expr_or_assign	{ $$ = xxexprlist2($1, $3); }
	|	exprlist '\n'					{ $$ = $1;}
	;

sublist	:	sub							{ $$ = xxsublist1($1); }
	|	sublist cr ',' sub				{ $$ = xxsublist2($1,$4); }
	;

sub	:									{ $$ = xxsub0(); 				}
	|	expr							{ $$ = xxsub1($1, &@1); 		}
	|	SYMBOL EQ_ASSIGN 				{ $$ = xxsymsub0($1, &@1); 	modif_token( &@2, EQ_SUB ) ; modif_token( &@1, SYMBOL_SUB ) ; }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxsymsub1($1,$3, &@1); 	modif_token( &@2, EQ_SUB ) ; modif_token( &@1, SYMBOL_SUB ) ; }
	|	STR_CONST EQ_ASSIGN 			{ $$ = xxsymsub0($1, &@1); 	modif_token( &@2, EQ_SUB ) ; }
	|	STR_CONST EQ_ASSIGN expr		{ $$ = xxsymsub1($1,$3, &@1); 	modif_token( &@2, EQ_SUB ) ; }
	|	NULL_CONST EQ_ASSIGN 			{ $$ = xxnullsub0(&@1); 		modif_token( &@2, EQ_SUB ) ; }
	|	NULL_CONST EQ_ASSIGN expr		{ $$ = xxnullsub1($3, &@1); 	modif_token( &@2, EQ_SUB ) ; }
	;

formlist:								{ $$ = xxnullformal(); }
	|	SYMBOL							{ $$ = xxfirstformal0($1); 			modif_token( &@1, SYMBOL_FORMALS ) ; }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxfirstformal1($1,$3); 			modif_token( &@1, SYMBOL_FORMALS ) ; modif_token( &@2, EQ_FORMALS ) ; }
	|	formlist ',' SYMBOL				{ $$ = xxaddformal0($1,$3, &@3); 		modif_token( &@3, SYMBOL_FORMALS ) ; }
	|	formlist ',' SYMBOL EQ_ASSIGN expr
										{ $$ = xxaddformal1($1,$3,$5,&@3);		modif_token( &@3, SYMBOL_FORMALS ) ; modif_token( &@4, EQ_FORMALS ) ;}
	;

cr	:									{ EatLines = 1; }
	;
