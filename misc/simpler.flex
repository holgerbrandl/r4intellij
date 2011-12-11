package preValidator.parsers;

import java.lang.System;
import java.io.*;
import java_cup.runtime.Symbol;
import preValidator.utils.Debug;
import preValidator.errors.*;
import preValidator.documents.*;
import preValidator.types.*;
%%

%{
	StringBuffer string = new StringBuffer();

	public ParametricList ph;
	public DppDocManager dppDocManager;
	public ErrorList el;

	private String ph_name;
	private int ph_val;
	private String ph_value;
	private String ph_value2;
	private int ph_type=0;
	private int ph_begin;

	private int string_count = 0;
	private boolean in_facet=false;
	private boolean end_string=false;
	private int comment_count = 0;

	private int currentLine=0;
	private int currentNest=0;

	int quoteState=0;		// 1=single, 2=double


	private void addString(String str, int numLine)
	{
		if(currentNest==0)
			dppDocManager.addStringSubst(str,numLine);
		else
			dppDocManager.addStringSubst(str,currentLine);

	}
%}


%line
%char
%state COMMENT_STATE
%state STRING
%state PATTERN
%state PATTERN_STATE
%state MACRO_STATE
%state ATTLIST
%state ATTLISTSTRING
%public
%class PerLexer
%unicode
%notunix

BR_CHAR=[\r\n]
WHITE_SPACE_CHAR=[\r\n\ \t\b\012]

NEW_LINE=[\r(\n)(\r\n)]

O_BRACE=[\x7B]
C_BRACE=[\x7D]
O_SQUARE=[\x5B]
C_SQUARE=[\x5D]
O_PAR=[\x28]
C_PAR=[\x29]
SLASH=[\x2F]
BACKSLASH=[\x5C]
QUOTE=[\x22]
PIPE=[\xA6]|[\x7C]
LESS=[\x2D]
QUEST=[\x3F]
SINGLEQUOTE=[\x27]
DOT=[\x2E]

LETTER=		[A-Za-z]
NUMBER=		[0-9]([0-9])*
NAMECHAR=	{LETTER}|{NUMBER}|[-_:]|\x2E
NAME=		({LETTER}|[_:])({NAMECHAR})*

PARAMETRIC_REFERENCE= %{NAME};


COMMENT_TEXT="<!--" ~"-->"

StringCharacter = [^\"\\]
OctDigit          = [0-7]

PatternCharacter= [^/\\]

%%

<YYINITIAL,MACRO_STATE,ATTLIST> {WHITE_SPACE_CHAR}+ { addString(yytext(),yyline);}

/*
<YYINITIAL> "<!ATTLIST " {
				addString(yytext(),yyline);
				Debug.PerLexerPrint("BEGIN ATTLIST");
				yybegin(ATTLIST);
			}

*/
<YYINITIAL> "<!ENTITY %" {
							addString(yytext(),yyline);
							Debug.PerLexerPrint("Macro_state BEGIN "+yytext());
							ph_name="";
							ph_value="";
							ph_value2="";
							ph_type=0;
							ph_val=0;
							ph_begin=yychar;
							yybegin(MACRO_STATE);
						}

<YYINITIAL> {PARAMETRIC_REFERENCE}
			{
				String newVal=ph.getValue(yytext());
				if(newVal!=null)
				{
					if(currentNest==0)
					{
						currentLine=yyline;
					}
					yypushStream(new StringReader(newVal));
					currentNest++;
					Debug.PerLexerPrint("Start Parametric reference"+yytext()+" "+newVal+" "+yymoreStreams());
				}
			}

<<EOF>>        {
		Debug.PerLexerPrint("Stop Parametric reference"+yytext());
		if (yymoreStreams())
		{
			yypopStream();
			currentNest--;
		}
		else
			return null;
		}

<MACRO_STATE> {

	"SYSTEM"	{	addString(yytext(),yyline);Debug.PerLexerPrint(yystate()+" Macro_state 1 "+yytext()); ph_type=1;}
	"PUBLIC"	{	addString(yytext(),yyline);Debug.PerLexerPrint(yystate()+" Macro_state 2 "+yytext()); ph_type=2;}
	"NDATA"		{	addString(yytext(),yyline);Debug.PerLexerPrint(yystate()+" Macro_state 3 "+yytext()); ph_type=3;}

	{NAME}	{
		addString(yytext(),yyline);
				Debug.PerLexerPrint(yystate()+" Macro_state NAME "+yytext());
				ph_name=yytext();
			}


	{QUOTE}	{
				Debug.PerLexerPrint(yystate()+" Macro_state QUOTE ");

				quoteState=2;

				addString(yytext(),yyline);
				yybegin(STRING);
				string.setLength(0);
			}

	{SINGLEQUOTE}	{
				Debug.PerLexerPrint(yystate()+" Macro_state QUOTE ");

				quoteState=1;

				addString(yytext(),yyline);
				yybegin(STRING);
				string.setLength(0);
			}


	">"		{
				addString(yytext(),yyline);
				Debug.PerLexerPrint(yystate()+" Macro_state CLOSE");
				ph.put(ph_name,ph_value,ph_value2,ph_type,ph_begin);
				yybegin(YYINITIAL);
			}

	/* error cases */
	.		{
				System.out.println("Illegal macro character: <" + yytext() + "> ("+yyline+")");
			}
}




<ATTLIST>
{
	{QUOTE}		{
					addString(yytext(),yyline);
					Debug.PerLexerPrint("AT foundstring");
					yybegin(ATTLISTSTRING);}

	">"		{
				addString(yytext(),yyline);
				Debug.PerLexerPrint("CLOSE AT");
				yybegin(YYINITIAL);
			}
	/* error cases */
	.		{
				//System.out.println("Illegal attlist character: <" + yytext() + "> ("+yyline+")");
				;
			}
}
<ATTLISTSTRING> {
	\"	{
			addString(yytext(),yyline);
			yybegin(ATTLIST);
		}

  {StringCharacter}+             { addString(yytext(),yyline);}

  /* escape sequences */
  "\\b"                          { addString(yytext(),yyline);}
  "\\t"                          { addString(yytext(),yyline);}
  "\\n"                          { addString(yytext(),yyline);}
  "\\f"                          { addString(yytext(),yyline);}
  "\\r"                          { addString(yytext(),yyline);}
  "\\\""                         { addString(yytext(),yyline);}
  "\\'"                          { addString(yytext(),yyline);}
  "\\\\"                         { addString(yytext(),yyline);}
  \\[0-3]?{OctDigit}?{OctDigit}  { addString(yytext(),yyline);}

  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
}


<STRING> {
	{QUOTE} {

			if(quoteState==2)
			{
				Debug.PerLexerPrint(yystate()+" Macro_state STRING " +string.toString());

				if(ph_val==0)
				{
					ph_value=string.toString();
					ph_val++;
				}
				else if (ph_val==1)
				{
					ph_value2=string.toString();
					ph_val++;
				}
				else
					System.out.println("Error: value not attended at line "+yyline);


				this.addString(string.toString()+yytext(),yyline);

				yybegin(MACRO_STATE);
			}
			else
			{
				string.append( '"' );
			}
		}

		{SINGLEQUOTE} {

			if(quoteState==1)
			{
				Debug.PerLexerPrint(yystate()+" Macro_state STRING " +string.toString());

				if(ph_val==0)
				{
					ph_value=string.toString();
					ph_val++;
				}
				else if (ph_val==1)
				{
					ph_value2=string.toString();
					ph_val++;
				}
				else
					System.out.println("Error: value not attended at line "+yyline);


				this.addString(string.toString()+yytext(),yyline);

				yybegin(MACRO_STATE);
			}
			else
			{
				string.append( '\'' );
			}
		}

  {StringCharacter}+             { string.append( yytext() ); }

  /* escape sequences */
  "\\b"                          { string.append( '\b' ); }
  "\\t"                          { string.append( '\t' ); }
  "\\n"                          { string.append( '\n' ); }
  "\\f"                          { string.append( '\f' ); }
  "\\r"                          { string.append( '\r' ); }
  "\\\""                         { string.append( '\"' ); }
  "\\'"                          { string.append( '\'' ); }
  "\\\\"                         { string.append( '\\' ); }
  \\[0-3]?{OctDigit}?{OctDigit}  { char val = (char) Integer.parseInt(yytext().substring(1),8);
                        				   string.append( val ); }

  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
}


<YYINITIAL> 	{SLASH}		{ yybegin(PATTERN); string.setLength(0); }

<PATTERN> {
	"/"                             {
										this.addString(string.toString()+yytext(),yyline);
										Debug.PerLexerPrint("MA " +string.toString());
										yybegin(YYINITIAL);}

  {PatternCharacter}+             { string.append( yytext() ); }

  /* escape sequences */
  "\\b"                          { string.append( "\\b" ); }
  "\\t"                          { string.append( "\\t" ); }
  "\\n"                          { string.append( "\\n" ); }
  "\\f"                          { string.append( "\\f" ); }
  "\\r"                          { string.append( "\\r" ); }
  \\{PatternCharacter}           { string.append( yytext() ); }
  "\\\\"                         { string.append( "\\\\" ); }
  "\\/"                          { string.append( "\\/" ); }
  \\[0-3]?{OctDigit}?{OctDigit}  { char val = (char) Integer.parseInt(yytext().substring(1),8);
                        				   string.append( val ); }

  /* error cases */
  \\.                            { throw new RuntimeException("Illegal escape sequence \""+yytext()+"\""); }
}


<YYINITIAL> {COMMENT_TEXT} {
	addString(yytext(),yyline);
	//System.out.println(yytext());
	;
	}




<YYINITIAL> . {
			//addString(yytext(),yyline);
			;

}

  /* string literal */


