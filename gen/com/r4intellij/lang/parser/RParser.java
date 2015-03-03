/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;

import static com.r4intellij.lang.parser.GrammarParserUtil.*;
import static com.r4intellij.psi.RTypes.*;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class RParser implements PsiParser {

    public ASTNode parse(IElementType t, PsiBuilder b) {
        parseLight(t, b);
        return b.getTreeBuilt();
    }


    public void parseLight(IElementType t, PsiBuilder b) {
        boolean r;
        b = adapt_builder_(t, b, this, null);
        Marker m = enter_section_(b, 0, _COLLAPSE_, null);
        if (t == R_COMMAND) {
            r = command(b, 0);
    } else if (t == R_COND) {
            r = cond(b, 0);
    } else if (t == R_DOCUMENT) {
            r = document(b, 0);
    } else if (t == R_EXPR) {
            r = expr(b, 0);
    } else if (t == R_EXPR_OR_ASSIGN) {
            r = expr_or_assign(b, 0);
    } else if (t == R_EXPRLIST) {
            r = exprlist(b, 0);
    } else if (t == R_FD_ARGUMENT) {
            r = fd_argument(b, 0);
    } else if (t == R_FORCOND) {
            r = forcond(b, 0);
    } else if (t == R_FUNCALL) {
            r = funcall(b, 0);
    } else if (t == R_FUNDEF) {
            r = fundef(b, 0);
    } else if (t == R_FUNDEF_ARGS) {
            r = fundef_args(b, 0);
    } else if (t == R_SECTION) {
            r = section(b, 0);
    } else if (t == R_STRING_LITERAL) {
            r = string_literal(b, 0);
    } else if (t == R_SUB) {
            r = sub(b, 0);
    } else if (t == R_SUBLIST) {
            r = sublist(b, 0);
    } else if (t == R_VARIABLE) {
            r = variable(b, 0);
    } else {
            r = parse_root_(t, b, 0);
        }
        exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
    }


    protected boolean parse_root_(IElementType t, PsiBuilder b, int l) {
        return document(b, l + 1);
    }


    /* ********************************************************** */
    // EOL | COMMENT | section |  expr_or_assign ';'?
    public static boolean command(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "command")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _NONE_, "<command>");
        r = consumeToken(b, R_EOL);
        if (!r) r = consumeToken(b, R_COMMENT);
        if (!r) r = section(b, l + 1);
        if (!r) r = command_3(b, l + 1);
        exit_section_(b, l, m, R_COMMAND, r, false, null);
        return r;
    }


    // expr_or_assign ';'?
    private static boolean command_3(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "command_3")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = expr_or_assign(b, l + 1);
        r = r && command_3_1(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // ';'?
    private static boolean command_3_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "command_3_1")) return false;
        consumeToken(b, R_SEMICOLON);
        return true;
    }


    /* ********************************************************** */
    // '(' expr ')'
    public static boolean cond(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "cond")) return false;
        if (!nextTokenIs(b, R_LEFT_PAREN)) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LEFT_PAREN);
        r = r && expr(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_PAREN);
        exit_section_(b, m, R_COND, r);
        return r;
    }


    /* ********************************************************** */
    // command*
    public static boolean document(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "document")) return false;
        Marker m = enter_section_(b, l, _NONE_, "<document>");
        int c = current_position_(b);
        while (true) {
            if (!command(b, l + 1)) break;
            if (!empty_element_parsed_guard_(b, "document", c)) break;
            c = current_position_(b);
    }
        exit_section_(b, l, m, R_DOCUMENT, true, false, null);
        return true;
    }


    /* ********************************************************** */
    // EOL* (
    //     NUM_CONST |
    //     NULL_CONST |
    //     SYMBOL_FORMALS |
    //     string_literal | // todo remove this as it is redundant with the last line optionalized with ()?
    //     funcall |
    //     variable (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST)? |
    //     '{' exprlist '}' |
    //     '(' expr_or_assign ')' |
    //     '-' expr |
    //     '+' expr |
    //     '!' expr |
    //     '~' expr |
    //     '?' expr |
    //     fundef |
    //     IF cond expr_or_assign [ELSE expr_or_assign] |
    //     FOR forcond expr_or_assign |
    //     WHILE cond expr_or_assign |
    //     REPEAT expr_or_assign |
    //     NEXT |
    //     BREAK) // end of first half
    //     (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | GLOBAL_LEFT_ASSIGN | GLOBAL_RIGHT_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    //     '(' sublist? ')' |
    //     '[[' sublist ']]' |
    //     '[' ','? sublist ','? ']' |
    //     '$' ( SYMBOL | STR_CONST) |
    //     '@' ( SYMBOL | STR_CONST )
    //     )*
    public static boolean expr(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _COLLAPSE_, "<expr>");
        r = expr_0(b, l + 1);
        r = r && expr_1(b, l + 1);
        r = r && expr_2(b, l + 1);
        exit_section_(b, l, m, R_EXPR, r, false, null);
        return r;
    }


    // EOL*
    private static boolean expr_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_0")) return false;
        int c = current_position_(b);
        while (true) {
            if (!consumeToken(b, R_EOL)) break;
            if (!empty_element_parsed_guard_(b, "expr_0", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    // NUM_CONST |
    //     NULL_CONST |
    //     SYMBOL_FORMALS |
    //     string_literal | // todo remove this as it is redundant with the last line optionalized with ()?
    //     funcall |
    //     variable (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST)? |
    //     '{' exprlist '}' |
    //     '(' expr_or_assign ')' |
    //     '-' expr |
    //     '+' expr |
    //     '!' expr |
    //     '~' expr |
    //     '?' expr |
    //     fundef |
    //     IF cond expr_or_assign [ELSE expr_or_assign] |
    //     FOR forcond expr_or_assign |
    //     WHILE cond expr_or_assign |
    //     REPEAT expr_or_assign |
    //     NEXT |
    //     BREAK
    private static boolean expr_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_NUM_CONST);
        if (!r) r = consumeToken(b, R_NULL_CONST);
        if (!r) r = consumeToken(b, R_SYMBOL_FORMALS);
        if (!r) r = string_literal(b, l + 1);
        if (!r) r = funcall(b, l + 1);
        if (!r) r = expr_1_5(b, l + 1);
        if (!r) r = expr_1_6(b, l + 1);
        if (!r) r = expr_1_7(b, l + 1);
        if (!r) r = expr_1_8(b, l + 1);
        if (!r) r = expr_1_9(b, l + 1);
        if (!r) r = expr_1_10(b, l + 1);
        if (!r) r = expr_1_11(b, l + 1);
        if (!r) r = expr_1_12(b, l + 1);
        if (!r) r = fundef(b, l + 1);
        if (!r) r = expr_1_14(b, l + 1);
        if (!r) r = expr_1_15(b, l + 1);
        if (!r) r = expr_1_16(b, l + 1);
        if (!r) r = expr_1_17(b, l + 1);
        if (!r) r = consumeToken(b, R_NEXT);
        if (!r) r = consumeToken(b, R_BREAK);
        exit_section_(b, m, null, r);
        return r;
    }


    // variable (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST)?
    private static boolean expr_1_5(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_5")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = variable(b, l + 1);
        r = r && expr_1_5_1(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST)?
    private static boolean expr_1_5_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_5_1")) return false;
        expr_1_5_1_0(b, l + 1);
        return true;
    }


    // NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST
    private static boolean expr_1_5_1_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_5_1_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = parseTokens(b, 0, R_NS_GET, R_SYMBOL);
        if (!r) r = parseTokens(b, 0, R_NS_GET, R_STR_CONST);
        if (!r) r = parseTokens(b, 0, R_NS_GET_INT, R_SYMBOL);
        if (!r) r = parseTokens(b, 0, R_NS_GET_INT, R_STR_CONST);
        exit_section_(b, m, null, r);
        return r;
    }


    // '{' exprlist '}'
    private static boolean expr_1_6(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_6")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LEFT_BRACE);
        r = r && exprlist(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_BRACE);
        exit_section_(b, m, null, r);
        return r;
    }


    // '(' expr_or_assign ')'
    private static boolean expr_1_7(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_7")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LEFT_PAREN);
        r = r && expr_or_assign(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_PAREN);
        exit_section_(b, m, null, r);
        return r;
    }


    // '-' expr
    private static boolean expr_1_8(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_8")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_ARITH_MINUS);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // '+' expr
    private static boolean expr_1_9(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_9")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_ARITH_PLUS);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // '!' expr
    private static boolean expr_1_10(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_10")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_NEGATION);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // '~' expr
    private static boolean expr_1_11(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_11")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_TILDE);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // '?' expr
    private static boolean expr_1_12(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_12")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_QUESTION);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // IF cond expr_or_assign [ELSE expr_or_assign]
    private static boolean expr_1_14(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_14")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_IF);
        r = r && cond(b, l + 1);
        r = r && expr_or_assign(b, l + 1);
        r = r && expr_1_14_3(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // [ELSE expr_or_assign]
    private static boolean expr_1_14_3(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_14_3")) return false;
        expr_1_14_3_0(b, l + 1);
        return true;
    }


    // ELSE expr_or_assign
    private static boolean expr_1_14_3_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_14_3_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_ELSE);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // FOR forcond expr_or_assign
    private static boolean expr_1_15(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_15")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_FOR);
        r = r && forcond(b, l + 1);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // WHILE cond expr_or_assign
    private static boolean expr_1_16(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_16")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_WHILE);
        r = r && cond(b, l + 1);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // REPEAT expr_or_assign
    private static boolean expr_1_17(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_1_17")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_REPEAT);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | GLOBAL_LEFT_ASSIGN | GLOBAL_RIGHT_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    //     '(' sublist? ')' |
    //     '[[' sublist ']]' |
    //     '[' ','? sublist ','? ']' |
    //     '$' ( SYMBOL | STR_CONST) |
    //     '@' ( SYMBOL | STR_CONST )
    //     )*
    private static boolean expr_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2")) return false;
        int c = current_position_(b);
        while (true) {
            if (!expr_2_0(b, l + 1)) break;
            if (!empty_element_parsed_guard_(b, "expr_2", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | GLOBAL_LEFT_ASSIGN | GLOBAL_RIGHT_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    //     '(' sublist? ')' |
    //     '[[' sublist ']]' |
    //     '[' ','? sublist ','? ']' |
    //     '$' ( SYMBOL | STR_CONST) |
    //     '@' ( SYMBOL | STR_CONST )
    private static boolean expr_2_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = expr_2_0_0(b, l + 1);
        if (!r) r = expr_2_0_1(b, l + 1);
        if (!r) r = expr_2_0_2(b, l + 1);
        if (!r) r = expr_2_0_3(b, l + 1);
        if (!r) r = expr_2_0_4(b, l + 1);
        if (!r) r = expr_2_0_5(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | GLOBAL_LEFT_ASSIGN | GLOBAL_RIGHT_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN ) expr
    private static boolean expr_2_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = expr_2_0_0_0(b, l + 1);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | GLOBAL_LEFT_ASSIGN | GLOBAL_RIGHT_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN
    private static boolean expr_2_0_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_0_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_COLON);
        if (!r) r = consumeToken(b, R_ARITH_PLUS);
        if (!r) r = consumeToken(b, R_ARITH_MINUS);
        if (!r) r = consumeToken(b, R_ARITH_MULT);
        if (!r) r = consumeToken(b, R_ARITH_DIV);
        if (!r) r = consumeToken(b, R_ARITH_EXPONENTIAION);
        if (!r) r = consumeToken(b, R_ARITH_MISC);
        if (!r) r = consumeToken(b, R_ARITH_MOD);
        if (!r) r = consumeToken(b, R_TILDE);
        if (!r) r = consumeToken(b, R_QUESTION);
        if (!r) r = consumeToken(b, R_LT);
        if (!r) r = consumeToken(b, R_LE);
        if (!r) r = consumeToken(b, R_EQ);
        if (!r) r = consumeToken(b, R_NE);
        if (!r) r = consumeToken(b, R_GE);
        if (!r) r = consumeToken(b, R_GT);
        if (!r) r = consumeToken(b, R_AND);
        if (!r) r = consumeToken(b, R_OR);
        if (!r) r = consumeToken(b, R_AND2);
        if (!r) r = consumeToken(b, R_OR2);
        if (!r) r = consumeToken(b, R_GLOBAL_LEFT_ASSIGN);
        if (!r) r = consumeToken(b, R_GLOBAL_RIGHT_ASSIGN);
        if (!r) r = consumeToken(b, R_LEFT_ASSIGN);
        if (!r) r = consumeToken(b, R_RIGHT_ASSIGN);
        exit_section_(b, m, null, r);
        return r;
    }


    // '(' sublist? ')'
    private static boolean expr_2_0_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LEFT_PAREN);
        r = r && expr_2_0_1_1(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_PAREN);
        exit_section_(b, m, null, r);
        return r;
    }


    // sublist?
    private static boolean expr_2_0_1_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_1_1")) return false;
        sublist(b, l + 1);
        return true;
    }


    // '[[' sublist ']]'
    private static boolean expr_2_0_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_2")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LBB);
        r = r && sublist(b, l + 1);
        r = r && consumeToken(b, R_RBB);
        exit_section_(b, m, null, r);
        return r;
    }


    // '[' ','? sublist ','? ']'
    private static boolean expr_2_0_3(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_3")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LEFT_BRACKET);
        r = r && expr_2_0_3_1(b, l + 1);
        r = r && sublist(b, l + 1);
        r = r && expr_2_0_3_3(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_BRACKET);
        exit_section_(b, m, null, r);
        return r;
    }


    // ','?
    private static boolean expr_2_0_3_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_3_1")) return false;
        consumeToken(b, R_COMMA);
        return true;
    }


    // ','?
    private static boolean expr_2_0_3_3(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_3_3")) return false;
        consumeToken(b, R_COMMA);
        return true;
    }


    // '$' ( SYMBOL | STR_CONST)
    private static boolean expr_2_0_4(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_4")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LIST_SUBSET);
        r = r && expr_2_0_4_1(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // SYMBOL | STR_CONST
    private static boolean expr_2_0_4_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_4_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SYMBOL);
        if (!r) r = consumeToken(b, R_STR_CONST);
        exit_section_(b, m, null, r);
        return r;
    }


    // '@' ( SYMBOL | STR_CONST )
    private static boolean expr_2_0_5(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_5")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SLOT);
        r = r && expr_2_0_5_1(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // SYMBOL | STR_CONST
    private static boolean expr_2_0_5_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_2_0_5_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SYMBOL);
        if (!r) r = consumeToken(b, R_STR_CONST);
        exit_section_(b, m, null, r);
        return r;
    }


    /* ********************************************************** */
    // EOL* ( expr [(EQ_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN | GLOBAL_RIGHT_ASSIGN | GLOBAL_LEFT_ASSIGN) expr_or_assign] )
    public static boolean expr_or_assign(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_or_assign")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _NONE_, "<expr or assign>");
        r = expr_or_assign_0(b, l + 1);
        r = r && expr_or_assign_1(b, l + 1);
        exit_section_(b, l, m, R_EXPR_OR_ASSIGN, r, false, null);
        return r;
    }


    // EOL*
    private static boolean expr_or_assign_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_or_assign_0")) return false;
        int c = current_position_(b);
        while (true) {
            if (!consumeToken(b, R_EOL)) break;
            if (!empty_element_parsed_guard_(b, "expr_or_assign_0", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    // expr [(EQ_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN | GLOBAL_RIGHT_ASSIGN | GLOBAL_LEFT_ASSIGN) expr_or_assign]
    private static boolean expr_or_assign_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_or_assign_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = expr(b, l + 1);
        r = r && expr_or_assign_1_1(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // [(EQ_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN | GLOBAL_RIGHT_ASSIGN | GLOBAL_LEFT_ASSIGN) expr_or_assign]
    private static boolean expr_or_assign_1_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_or_assign_1_1")) return false;
        expr_or_assign_1_1_0(b, l + 1);
        return true;
    }


    // (EQ_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN | GLOBAL_RIGHT_ASSIGN | GLOBAL_LEFT_ASSIGN) expr_or_assign
    private static boolean expr_or_assign_1_1_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_or_assign_1_1_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = expr_or_assign_1_1_0_0(b, l + 1);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // EQ_ASSIGN | LEFT_ASSIGN | RIGHT_ASSIGN | GLOBAL_RIGHT_ASSIGN | GLOBAL_LEFT_ASSIGN
    private static boolean expr_or_assign_1_1_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "expr_or_assign_1_1_0_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_EQ_ASSIGN);
        if (!r) r = consumeToken(b, R_LEFT_ASSIGN);
        if (!r) r = consumeToken(b, R_RIGHT_ASSIGN);
        if (!r) r = consumeToken(b, R_GLOBAL_RIGHT_ASSIGN);
        if (!r) r = consumeToken(b, R_GLOBAL_LEFT_ASSIGN);
        exit_section_(b, m, null, r);
        return r;
    }


    /* ********************************************************** */
    // [(expr_or_assign) (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*] EOL*
    public static boolean exprlist(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _NONE_, "<exprlist>");
        r = exprlist_0(b, l + 1);
        r = r && exprlist_1(b, l + 1);
        exit_section_(b, l, m, R_EXPRLIST, r, false, null);
        return r;
    }


    // [(expr_or_assign) (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*]
    private static boolean exprlist_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_0")) return false;
        exprlist_0_0(b, l + 1);
        return true;
    }


    // (expr_or_assign) (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*
    private static boolean exprlist_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_0_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = exprlist_0_0_0(b, l + 1);
        r = r && exprlist_0_0_1(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // (expr_or_assign)
    private static boolean exprlist_0_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_0_0_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*
    private static boolean exprlist_0_0_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_0_0_1")) return false;
        int c = current_position_(b);
        while (true) {
            if (!exprlist_0_0_1_0(b, l + 1)) break;
            if (!empty_element_parsed_guard_(b, "exprlist_0_0_1", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    // ';' expr_or_assign | EOL expr_or_assign |';' |  EOL
    private static boolean exprlist_0_0_1_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_0_0_1_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = exprlist_0_0_1_0_0(b, l + 1);
        if (!r) r = exprlist_0_0_1_0_1(b, l + 1);
        if (!r) r = consumeToken(b, R_SEMICOLON);
        if (!r) r = consumeToken(b, R_EOL);
        exit_section_(b, m, null, r);
        return r;
    }


    // ';' expr_or_assign
    private static boolean exprlist_0_0_1_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_0_0_1_0_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SEMICOLON);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // EOL expr_or_assign
    private static boolean exprlist_0_0_1_0_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_0_0_1_0_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_EOL);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // EOL*
    private static boolean exprlist_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "exprlist_1")) return false;
        int c = current_position_(b);
        while (true) {
            if (!consumeToken(b, R_EOL)) break;
            if (!empty_element_parsed_guard_(b, "exprlist_1", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    /* ********************************************************** */
    // SYMBOL '=' expr
    //         | SYMBOL
    //         | STR_CONST '=' expr
    //         | STR_CONST
    //         | SYMBOL_FORMALS
    public static boolean fd_argument(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fd_argument")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _NONE_, "<fd argument>");
        r = fd_argument_0(b, l + 1);
        if (!r) r = consumeToken(b, R_SYMBOL);
        if (!r) r = fd_argument_2(b, l + 1);
        if (!r) r = consumeToken(b, R_STR_CONST);
        if (!r) r = consumeToken(b, R_SYMBOL_FORMALS);
        exit_section_(b, l, m, R_FD_ARGUMENT, r, false, null);
        return r;
    }


    // SYMBOL '=' expr
    private static boolean fd_argument_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fd_argument_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SYMBOL);
        r = r && consumeToken(b, R_EQ_ASSIGN);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // STR_CONST '=' expr
    private static boolean fd_argument_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fd_argument_2")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_STR_CONST);
        r = r && consumeToken(b, R_EQ_ASSIGN);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    /* ********************************************************** */
    // '(' variable IN expr ')'
    public static boolean forcond(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "forcond")) return false;
        if (!nextTokenIs(b, R_LEFT_PAREN)) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_LEFT_PAREN);
        r = r && variable(b, l + 1);
        r = r && consumeToken(b, R_IN);
        r = r && expr(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_PAREN);
        exit_section_(b, m, R_FORCOND, r);
        return r;
    }


    /* ********************************************************** */
    // variable '(' sublist? ')'
    public static boolean funcall(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "funcall")) return false;
        if (!nextTokenIs(b, R_SYMBOL)) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = variable(b, l + 1);
        r = r && consumeToken(b, R_LEFT_PAREN);
        r = r && funcall_2(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_PAREN);
        exit_section_(b, m, R_FUNCALL, r);
        return r;
    }


    // sublist?
    private static boolean funcall_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "funcall_2")) return false;
        sublist(b, l + 1);
        return true;
    }


    /* ********************************************************** */
    // FUNCTION '(' fundef_args? ')'  expr_or_assign
    public static boolean fundef(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fundef")) return false;
        if (!nextTokenIs(b, R_FUNCTION)) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_FUNCTION);
        r = r && consumeToken(b, R_LEFT_PAREN);
        r = r && fundef_2(b, l + 1);
        r = r && consumeToken(b, R_RIGHT_PAREN);
        r = r && expr_or_assign(b, l + 1);
        exit_section_(b, m, R_FUNDEF, r);
        return r;
    }


    // fundef_args?
    private static boolean fundef_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fundef_2")) return false;
        fundef_args(b, l + 1);
        return true;
    }


    /* ********************************************************** */
    // fd_argument (EOL? ',' EOL? fd_argument)*
    public static boolean fundef_args(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fundef_args")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _NONE_, "<fundef args>");
        r = fd_argument(b, l + 1);
        r = r && fundef_args_1(b, l + 1);
        exit_section_(b, l, m, R_FUNDEF_ARGS, r, false, null);
        return r;
    }


    // (EOL? ',' EOL? fd_argument)*
    private static boolean fundef_args_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fundef_args_1")) return false;
        int c = current_position_(b);
        while (true) {
            if (!fundef_args_1_0(b, l + 1)) break;
            if (!empty_element_parsed_guard_(b, "fundef_args_1", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    // EOL? ',' EOL? fd_argument
    private static boolean fundef_args_1_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fundef_args_1_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = fundef_args_1_0_0(b, l + 1);
        r = r && consumeToken(b, R_COMMA);
        r = r && fundef_args_1_0_2(b, l + 1);
        r = r && fd_argument(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // EOL?
    private static boolean fundef_args_1_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fundef_args_1_0_0")) return false;
        consumeToken(b, R_EOL);
        return true;
    }


    // EOL?
    private static boolean fundef_args_1_0_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "fundef_args_1_0_2")) return false;
        consumeToken(b, R_EOL);
        return true;
    }


    /* ********************************************************** */
    // SECTION_COMMENT
    public static boolean section(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "section")) return false;
        if (!nextTokenIs(b, R_SECTION_COMMENT)) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SECTION_COMMENT);
        exit_section_(b, m, R_SECTION, r);
        return r;
    }


    /* ********************************************************** */
    // STR_CONST
    public static boolean string_literal(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "string_literal")) return false;
        if (!nextTokenIs(b, R_STR_CONST)) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_STR_CONST);
        exit_section_(b, m, R_STRING_LITERAL, r);
        return r;
    }


    /* ********************************************************** */
    // EOL*
    //     (SYMBOL '=' expr
    // //   | STR_CONST '=' expr
    // //   | NULL_CONST '=' expr
    //     | STR_CONST EQ_ASSIGN expr
    //     | SYMBOL_FORMALS
    //    | expr ) EOL*
    public static boolean sub(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sub")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _NONE_, "<sub>");
        r = sub_0(b, l + 1);
        r = r && sub_1(b, l + 1);
        r = r && sub_2(b, l + 1);
        exit_section_(b, l, m, R_SUB, r, false, null);
        return r;
    }


    // EOL*
    private static boolean sub_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sub_0")) return false;
        int c = current_position_(b);
        while (true) {
            if (!consumeToken(b, R_EOL)) break;
            if (!empty_element_parsed_guard_(b, "sub_0", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    // SYMBOL '=' expr
    // //   | STR_CONST '=' expr
    // //   | NULL_CONST '=' expr
    //     | STR_CONST EQ_ASSIGN expr
    //     | SYMBOL_FORMALS
    //    | expr
    private static boolean sub_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sub_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = sub_1_0(b, l + 1);
        if (!r) r = sub_1_1(b, l + 1);
        if (!r) r = consumeToken(b, R_SYMBOL_FORMALS);
        if (!r) r = expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // SYMBOL '=' expr
    private static boolean sub_1_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sub_1_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SYMBOL);
        r = r && consumeToken(b, R_EQ_ASSIGN);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // STR_CONST EQ_ASSIGN expr
    private static boolean sub_1_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sub_1_1")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeTokens(b, 0, R_STR_CONST, R_EQ_ASSIGN);
        r = r && expr(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // EOL*
    private static boolean sub_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sub_2")) return false;
        int c = current_position_(b);
        while (true) {
            if (!consumeToken(b, R_EOL)) break;
            if (!empty_element_parsed_guard_(b, "sub_2", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    /* ********************************************************** */
    // sub (EOL? ',' EOL? sub)*
    public static boolean sublist(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sublist")) return false;
        boolean r;
        Marker m = enter_section_(b, l, _NONE_, "<sublist>");
        r = sub(b, l + 1);
        r = r && sublist_1(b, l + 1);
        exit_section_(b, l, m, R_SUBLIST, r, false, null);
        return r;
    }


    // (EOL? ',' EOL? sub)*
    private static boolean sublist_1(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sublist_1")) return false;
        int c = current_position_(b);
        while (true) {
            if (!sublist_1_0(b, l + 1)) break;
            if (!empty_element_parsed_guard_(b, "sublist_1", c)) break;
            c = current_position_(b);
    }
        return true;
    }


    // EOL? ',' EOL? sub
    private static boolean sublist_1_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sublist_1_0")) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = sublist_1_0_0(b, l + 1);
        r = r && consumeToken(b, R_COMMA);
        r = r && sublist_1_0_2(b, l + 1);
        r = r && sub(b, l + 1);
        exit_section_(b, m, null, r);
        return r;
    }


    // EOL?
    private static boolean sublist_1_0_0(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sublist_1_0_0")) return false;
        consumeToken(b, R_EOL);
        return true;
    }


    // EOL?
    private static boolean sublist_1_0_2(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "sublist_1_0_2")) return false;
        consumeToken(b, R_EOL);
        return true;
    }


    /* ********************************************************** */
    // SYMBOL
    public static boolean variable(PsiBuilder b, int l) {
        if (!recursion_guard_(b, l, "variable")) return false;
        if (!nextTokenIs(b, R_SYMBOL)) return false;
        boolean r;
        Marker m = enter_section_(b);
        r = consumeToken(b, R_SYMBOL);
        exit_section_(b, m, R_VARIABLE, r);
        return r;
    }

}
