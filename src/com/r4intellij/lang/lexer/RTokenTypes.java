/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang.lexer;

import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import com.r4intellij.file.RFileType;


/**
 * Identifies all of the token types (at least, the ones we'll care about) in Arc.
 * Used by the lexer to break a R source file down into tokens.
 *
 * @author Holger Brandl
 */
public interface RTokenTypes {

    IFileElementType FILE = new IFileElementType(RFileType.R_LANGUAGE);

    IElementType BAD_CHARACTER = TokenType.BAD_CHARACTER;

    IElementType CONSTANT = new RTokenType("Constant");
    IElementType VARIABLE = new RTokenType("variable");
    IElementType INTERNAL_COMMAND = new RTokenType("internal R command");
    IElementType INTEGER_LITERAL = new RTokenType("int literal");
    IElementType STRING_LITERAL = new RTokenType("String");
    IElementType COMMENT = new RTokenType("Comment");
    IElementType NUMBER = new RTokenType("number");
    IElementType IDENTIFIER = new RTokenType("word");


    // squares
    IElementType LEFT_PAREN = new RTokenType("(");
    IElementType RIGHT_PAREN = new RTokenType(")");
    IElementType LEFT_CURLY = new RTokenType("{");
    IElementType RIGHT_CURLY = new RTokenType("}");
    IElementType LEFT_SQUARE = new RTokenType("[ (left square)");
    IElementType RIGHT_SQUARE = new RTokenType("] (right square)");

    // R reserved keywords, in alphabetic order
    IElementType CASE_KEYWORD = new RTokenType("case"); //case
    IElementType DO_KEYWORD = new RTokenType("do"); //do
    IElementType DONE_KEYWORD = new RTokenType("done"); //done
    IElementType ELIF_KEYWORD = new RTokenType("elif");//elif
    IElementType ELSE_KEYWORD = new RTokenType("else");//else
    IElementType ESAC_KEYWORD = new RTokenType("esac"); //esac
    IElementType FI_KEYWORD = new RTokenType("fi");//fi
    IElementType FOR_KEYWORD = new RTokenType("for");//for
    IElementType FUNCTION_KEYWORD = new RTokenType("function");//function
    IElementType IF_KEYWORD = new RTokenType("if");//if
    IElementType IN_KEYWORD = new RTokenType("in");//in
    IElementType SELECT_KEYWORD = new RTokenType("select");//select
    IElementType THEN_KEYWORD = new RTokenType("then");//then
    IElementType UNTIL_KEYWORD = new RTokenType("until");//until
    IElementType WHILE_KEYWORD = new RTokenType("while");//while
    IElementType TIME_KEYWORD = new RTokenType("time");//time
    IElementType BRACKET_KEYWORD = new RTokenType("[[ (left bracket)");//[[
    IElementType _BRACKET_KEYWORD = new RTokenType("]] (right bracket)");//]]

    TokenSet keywords = TokenSet.create(CASE_KEYWORD, DO_KEYWORD, DONE_KEYWORD,
            ELIF_KEYWORD, ELSE_KEYWORD, ESAC_KEYWORD, FI_KEYWORD, FOR_KEYWORD, FUNCTION_KEYWORD,
            IF_KEYWORD, IN_KEYWORD, SELECT_KEYWORD, THEN_KEYWORD, UNTIL_KEYWORD, WHILE_KEYWORD,
            TIME_KEYWORD, BRACKET_KEYWORD, _BRACKET_KEYWORD);

    // single characters
    IElementType COLON = new RTokenType(":");
    IElementType COMMA = new RTokenType(",");
    IElementType SEMICOLON = new RTokenType(";");
    IElementType DOT = new RTokenType(".");
    IElementType TILDE = new RTokenType("~");

    IElementType ASSIGNMENT = new RTokenType("assignment_word");
    IElementType LOG_OPERATOR = new RTokenType("logical operator");
    IElementType LIST_SUBSET = new RTokenType("$");
    IElementType VARARGS = new RTokenType("...");


    IElementType ARITH_PLUS = new RTokenType("+");//+
    IElementType ARITH_MINUS = new RTokenType("-");//+
    IElementType NEGATION = new RTokenType("!");
    IElementType ARITH_MULT = new RTokenType("*");//*
    IElementType ARITH_DIV = new RTokenType("/");// /
    IElementType ARITH_MOD = new RTokenType("%");//%
    IElementType ARITH_EXPONENTIAION = new RTokenType("^");
    IElementType ARITH_MISC = new RTokenType("%someop%");
}