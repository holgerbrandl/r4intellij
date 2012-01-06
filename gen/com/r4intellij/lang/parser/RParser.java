/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import com.intellij.lang.PsiParser;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.lang.parser.GrammarParserUtil.*;
import static com.r4intellij.psi.RTypes.*;


@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class RParser implements PsiParser {

    public static Logger LOG_ = Logger.getInstance("com.r4intellij.lang.parser.RParser");

    @NotNull
    public ASTNode parse(final IElementType root_, final PsiBuilder builder_) {
        int level_ = 0;
        boolean result_;
        if (root_ == R_COND) {
            result_ = cond(builder_, level_ + 1);
        } else if (root_ == R_EQUAL_ASSIGN) {
            result_ = equal_assign(builder_, level_ + 1);
        } else if (root_ == R_EXPR) {
            result_ = expr(builder_, level_ + 1);
        } else if (root_ == R_EXPR_OR_ASSIGN) {
            result_ = expr_or_assign(builder_, level_ + 1);
        } else if (root_ == R_EXPRLIST) {
            result_ = exprlist(builder_, level_ + 1);
        } else if (root_ == R_FORCOND) {
            result_ = forcond(builder_, level_ + 1);
        } else if (root_ == R_FORM) {
            result_ = form(builder_, level_ + 1);
        } else if (root_ == R_FORMLIST) {
            result_ = formlist(builder_, level_ + 1);
        } else if (root_ == R_FUNDEF) {
            result_ = fundef(builder_, level_ + 1);
        } else if (root_ == R_IFCOND) {
            result_ = ifcond(builder_, level_ + 1);
        } else if (root_ == R_PROG) {
            result_ = prog(builder_, level_ + 1);
        } else if (root_ == R_SUB) {
            result_ = sub(builder_, level_ + 1);
        } else if (root_ == R_SUBLIST) {
            result_ = sublist(builder_, level_ + 1);
        } else {
            Marker marker_ = builder_.mark();
            result_ = parseGrammar(builder_, level_ + 1, prog_parser_);
            while (builder_.getTokenType() != null) {
                builder_.advanceLexer();
            }
            marker_.done(root_);
        }
        return builder_.getTreeBuilt();
    }

    /* ********************************************************** */
    // '(' expr ')'
    public static boolean cond(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "cond")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (result_) {
            marker_.done(R_COND);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // expr EQ_ASSIGN expr_or_assign
    public static boolean equal_assign(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "equal_assign")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EQUAL_ASSIGN);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // ('{' exprlist '}' |
    // 	'(' expr_or_assign ')' |
    // 	'-' expr |
    // 	'+' expr |
    // 	'!' expr |
    // 	'~' expr |
    // 	'?' expr |
    // 	fundef |
    // 	IF ifcond EOL? expr_or_assign |
    // 	IF ifcond EOL? expr_or_assign ELSE EOL? expr_or_assign |
    // 	FOR forcond expr_or_assign |
    // 	WHILE cond expr_or_assign |
    // 	REPEAT expr_or_assign |
    // 	SYMBOL NS_GET SYMBOL |
    // 	SYMBOL NS_GET STR_CONST |
    // 	STR_CONST NS_GET SYMBOL |
    // 	STR_CONST NS_GET STR_CONST |
    // 	SYMBOL NS_GET_INT SYMBOL |
    // 	SYMBOL NS_GET_INT STR_CONST |
    // 	STR_CONST NS_GET_INT SYMBOL |
    // 	STR_CONST NS_GET_INT STR_CONST |
    // 	NUM_CONST |
    // 	STR_CONST |
    // 	NULL_CONST |
    // 	SYMBOL |
    // 	NEXT |
    // 	BREAK) // end of first half
    //     (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    // 	'(' sublist ')' |
    // 	'[[' sublist ']]' |
    // 	'[' ','? sublist ','? ']' |
    // 	'$' SYMBOL |
    // 	'$' STR_CONST |
    // 	'@' SYMBOL |
    // 	'@' STR_CONST
    // 	)*
    public static boolean expr(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_0(builder_, level_ + 1);
        result_ = result_ && expr_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPR);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // ('{' exprlist '}' |
    // 	'(' expr_or_assign ')' |
    // 	'-' expr |
    // 	'+' expr |
    // 	'!' expr |
    // 	'~' expr |
    // 	'?' expr |
    // 	fundef |
    // 	IF ifcond EOL? expr_or_assign |
    // 	IF ifcond EOL? expr_or_assign ELSE EOL? expr_or_assign |
    // 	FOR forcond expr_or_assign |
    // 	WHILE cond expr_or_assign |
    // 	REPEAT expr_or_assign |
    // 	SYMBOL NS_GET SYMBOL |
    // 	SYMBOL NS_GET STR_CONST |
    // 	STR_CONST NS_GET SYMBOL |
    // 	STR_CONST NS_GET STR_CONST |
    // 	SYMBOL NS_GET_INT SYMBOL |
    // 	SYMBOL NS_GET_INT STR_CONST |
    // 	STR_CONST NS_GET_INT SYMBOL |
    // 	STR_CONST NS_GET_INT STR_CONST |
    // 	NUM_CONST |
    // 	STR_CONST |
    // 	NULL_CONST |
    // 	SYMBOL |
    // 	NEXT |
    // 	BREAK)
    private static boolean expr_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0")) return false;
        return expr_0_0(builder_, level_ + 1);
    }

    // '{' exprlist '}' |
    // 	'(' expr_or_assign ')' |
    // 	'-' expr |
    // 	'+' expr |
    // 	'!' expr |
    // 	'~' expr |
    // 	'?' expr |
    // 	fundef |
    // 	IF ifcond EOL? expr_or_assign |
    // 	IF ifcond EOL? expr_or_assign ELSE EOL? expr_or_assign |
    // 	FOR forcond expr_or_assign |
    // 	WHILE cond expr_or_assign |
    // 	REPEAT expr_or_assign |
    // 	SYMBOL NS_GET SYMBOL |
    // 	SYMBOL NS_GET STR_CONST |
    // 	STR_CONST NS_GET SYMBOL |
    // 	STR_CONST NS_GET STR_CONST |
    // 	SYMBOL NS_GET_INT SYMBOL |
    // 	SYMBOL NS_GET_INT STR_CONST |
    // 	STR_CONST NS_GET_INT SYMBOL |
    // 	STR_CONST NS_GET_INT STR_CONST |
    // 	NUM_CONST |
    // 	STR_CONST |
    // 	NULL_CONST |
    // 	SYMBOL |
    // 	NEXT |
    // 	BREAK
    private static boolean expr_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_0_0_0(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_1(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_2(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_3(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_4(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_5(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_6(builder_, level_ + 1);
        if (!result_) result_ = fundef(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_8(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_9(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_10(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_11(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_12(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_13(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_14(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_15(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_16(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_17(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_18(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_19(builder_, level_ + 1);
        if (!result_) result_ = expr_0_0_20(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_NUM_CONST);
        if (!result_) result_ = consumeToken(builder_, R_STR_CONST);
        if (!result_) result_ = consumeToken(builder_, R_NULL_CONST);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL);
        if (!result_) result_ = consumeToken(builder_, R_NEXT);
        if (!result_) result_ = consumeToken(builder_, R_BREAK);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '{' exprlist '}'
    private static boolean expr_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_BRACE);
        result_ = result_ && exprlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_BRACE);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '(' expr_or_assign ')'
    private static boolean expr_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '-' expr
    private static boolean expr_0_0_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_ARITH_MINUS);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '+' expr
    private static boolean expr_0_0_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_3")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_ARITH_PLUS);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '!' expr
    private static boolean expr_0_0_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_4")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NEGATION);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '~' expr
    private static boolean expr_0_0_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_5")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_TILDE);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '?' expr
    private static boolean expr_0_0_6(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_6")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_QUESTION);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // IF ifcond EOL? expr_or_assign
    private static boolean expr_0_0_8(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_8")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_IF);
        result_ = result_ && ifcond(builder_, level_ + 1);
        result_ = result_ && expr_0_0_8_2(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // EOL?
    private static boolean expr_0_0_8_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_8_2")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }

    // IF ifcond EOL? expr_or_assign ELSE EOL? expr_or_assign
    private static boolean expr_0_0_9(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_9")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_IF);
        result_ = result_ && ifcond(builder_, level_ + 1);
        result_ = result_ && expr_0_0_9_2(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_ELSE);
        result_ = result_ && expr_0_0_9_5(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // EOL?
    private static boolean expr_0_0_9_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_9_2")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }

    // EOL?
    private static boolean expr_0_0_9_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_9_5")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }

    // FOR forcond expr_or_assign
    private static boolean expr_0_0_10(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_10")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_FOR);
        result_ = result_ && forcond(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // WHILE cond expr_or_assign
    private static boolean expr_0_0_11(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_11")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_WHILE);
        result_ = result_ && cond(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // REPEAT expr_or_assign
    private static boolean expr_0_0_12(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_12")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_REPEAT);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // SYMBOL NS_GET SYMBOL
    private static boolean expr_0_0_13(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_13")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_NS_GET);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // SYMBOL NS_GET STR_CONST
    private static boolean expr_0_0_14(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_14")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_NS_GET);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // STR_CONST NS_GET SYMBOL
    private static boolean expr_0_0_15(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_15")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        result_ = result_ && consumeToken(builder_, R_NS_GET);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // STR_CONST NS_GET STR_CONST
    private static boolean expr_0_0_16(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_16")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        result_ = result_ && consumeToken(builder_, R_NS_GET);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // SYMBOL NS_GET_INT SYMBOL
    private static boolean expr_0_0_17(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_17")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_NS_GET_INT);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // SYMBOL NS_GET_INT STR_CONST
    private static boolean expr_0_0_18(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_18")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_NS_GET_INT);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // STR_CONST NS_GET_INT SYMBOL
    private static boolean expr_0_0_19(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_19")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        result_ = result_ && consumeToken(builder_, R_NS_GET_INT);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // STR_CONST NS_GET_INT STR_CONST
    private static boolean expr_0_0_20(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0_0_20")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        result_ = result_ && consumeToken(builder_, R_NS_GET_INT);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    // 	'(' sublist ')' |
    // 	'[[' sublist ']]' |
    // 	'[' ','? sublist ','? ']' |
    // 	'$' SYMBOL |
    // 	'$' STR_CONST |
    // 	'@' SYMBOL |
    // 	'@' STR_CONST
    // 	)*
    private static boolean expr_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!expr_1_0(builder_, level_ + 1)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "expr_1");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }

    // (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    // 	'(' sublist ')' |
    // 	'[[' sublist ']]' |
    // 	'[' ','? sublist ','? ']' |
    // 	'$' SYMBOL |
    // 	'$' STR_CONST |
    // 	'@' SYMBOL |
    // 	'@' STR_CONST
    // 	)
    private static boolean expr_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0")) return false;
        return expr_1_0_0(builder_, level_ + 1);
    }

    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    // 	'(' sublist ')' |
    // 	'[[' sublist ']]' |
    // 	'[' ','? sublist ','? ']' |
    // 	'$' SYMBOL |
    // 	'$' STR_CONST |
    // 	'@' SYMBOL |
    // 	'@' STR_CONST
    private static boolean expr_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_1_0_0_0(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_0_1(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_0_2(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_0_3(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_0_4(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_0_5(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_0_6(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_0_7(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr
    private static boolean expr_1_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_1_0_0_0_0(builder_, level_ + 1);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN )
    private static boolean expr_1_0_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_0_0")) return false;
        return expr_1_0_0_0_0_0(builder_, level_ + 1);
    }

    // ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN
    private static boolean expr_1_0_0_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_0_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_COLON);
        if (!result_) result_ = consumeToken(builder_, R_ARITH_PLUS);
        if (!result_) result_ = consumeToken(builder_, R_ARITH_MINUS);
        if (!result_) result_ = consumeToken(builder_, R_ARITH_MULT);
        if (!result_) result_ = consumeToken(builder_, R_ARITH_DIV);
        if (!result_) result_ = consumeToken(builder_, R_ARITH_EXPONENTIAION);
        if (!result_) result_ = consumeToken(builder_, R_ARITH_MISC);
        if (!result_) result_ = consumeToken(builder_, R_ARITH_MOD);
        if (!result_) result_ = consumeToken(builder_, R_TILDE);
        if (!result_) result_ = consumeToken(builder_, R_QUESTION);
        if (!result_) result_ = consumeToken(builder_, R_LT);
        if (!result_) result_ = consumeToken(builder_, R_LE);
        if (!result_) result_ = consumeToken(builder_, R_EQ);
        if (!result_) result_ = consumeToken(builder_, R_NE);
        if (!result_) result_ = consumeToken(builder_, R_GE);
        if (!result_) result_ = consumeToken(builder_, R_GT);
        if (!result_) result_ = consumeToken(builder_, R_AND);
        if (!result_) result_ = consumeToken(builder_, R_OR);
        if (!result_) result_ = consumeToken(builder_, R_AND2);
        if (!result_) result_ = consumeToken(builder_, R_OR2);
        if (!result_) result_ = consumeToken(builder_, R_LEFT_ASSIGN);
        if (!result_) result_ = consumeToken(builder_, R_RIGHT_ASSIGN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '(' sublist ')'
    private static boolean expr_1_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && sublist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '[[' sublist ']]'
    private static boolean expr_1_0_0_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LBB);
        result_ = result_ && sublist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RBB);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '[' ','? sublist ','? ']'
    private static boolean expr_1_0_0_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_3")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_BRACKET);
        result_ = result_ && expr_1_0_0_3_1(builder_, level_ + 1);
        result_ = result_ && sublist(builder_, level_ + 1);
        result_ = result_ && expr_1_0_0_3_3(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_BRACKET);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // ','?
    private static boolean expr_1_0_0_3_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_3_1")) return false;
        consumeToken(builder_, R_COMMA);
        return true;
    }

    // ','?
    private static boolean expr_1_0_0_3_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_3_3")) return false;
        consumeToken(builder_, R_COMMA);
        return true;
    }

    // '$' SYMBOL
    private static boolean expr_1_0_0_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_4")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LIST_SUBSET);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '$' STR_CONST
    private static boolean expr_1_0_0_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_5")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LIST_SUBSET);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '@' SYMBOL
    private static boolean expr_1_0_0_6(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_6")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SLOT);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // '@' STR_CONST
    private static boolean expr_1_0_0_7(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_0_7")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SLOT);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // equal_assign | expr
    public static boolean expr_or_assign(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = equal_assign(builder_, level_ + 1);
        if (!result_) result_ = expr(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPR_OR_ASSIGN);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // !(EOL|';')
    static boolean expr_or_assign_recover(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_recover")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        enterErrorRecordingSection(builder_, level_, _SECTION_NOT_);
        result_ = !expr_or_assign_recover_0(builder_, level_ + 1);
        marker_.rollbackTo();
        result_ = exitErrorRecordingSection(builder_, result_, level_, false, _SECTION_NOT_, null);
        return result_;
    }

    // (EOL|';')
    private static boolean expr_or_assign_recover_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_recover_0")) return false;
        return expr_or_assign_recover_0_0(builder_, level_ + 1);
    }

    // EOL|';'
    private static boolean expr_or_assign_recover_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_recover_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_EOL);
        if (!result_) result_ = consumeToken(builder_, R_SEMICOLON);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // EOL?  expr_or_assign (';' expr_or_assign | EOL expr_or_assign |';' )*
    public static boolean exprlist(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist_0(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        result_ = result_ && exprlist_2(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPRLIST);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // EOL?
    private static boolean exprlist_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }

    // (';' expr_or_assign | EOL expr_or_assign |';' )*
    private static boolean exprlist_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_2")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!exprlist_2_0(builder_, level_ + 1)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "exprlist_2");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }

    // (';' expr_or_assign | EOL expr_or_assign |';' )
    private static boolean exprlist_2_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_2_0")) return false;
        return exprlist_2_0_0(builder_, level_ + 1);
    }

    // ';' expr_or_assign | EOL expr_or_assign |';'
    private static boolean exprlist_2_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_2_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist_2_0_0_0(builder_, level_ + 1);
        if (!result_) result_ = exprlist_2_0_0_1(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_SEMICOLON);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // ';' expr_or_assign
    private static boolean exprlist_2_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_2_0_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SEMICOLON);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // EOL expr_or_assign
    private static boolean exprlist_2_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_2_0_0_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_EOL);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // '(' SYMBOL IN expr ')'
    public static boolean forcond(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "forcond")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_IN);
        result_ = result_ && expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (result_) {
            marker_.done(R_FORCOND);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // SYMBOL '=' expr
    //         | SYMBOL
    //         | STR_CONST '=' expr
    //         | STR_CONST
    //         | SYMBOL_FORMALS
    public static boolean form(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "form")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = form_0(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL);
        if (!result_) result_ = form_2(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_STR_CONST);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL_FORMALS);
        if (result_) {
            marker_.done(R_FORM);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // SYMBOL '=' expr
    private static boolean form_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "form_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // STR_CONST '=' expr
    private static boolean form_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "form_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // form (EOL? ',' EOL? form)*
    public static boolean formlist(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = form(builder_, level_ + 1);
        result_ = result_ && formlist_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_FORMLIST);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // (EOL? ',' EOL? form)*
    private static boolean formlist_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_1")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!formlist_1_0(builder_, level_ + 1)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "formlist_1");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }

    // (EOL? ',' EOL? form)
    private static boolean formlist_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_1_0")) return false;
        return formlist_1_0_0(builder_, level_ + 1);
    }

    // EOL? ',' EOL? form
    private static boolean formlist_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = formlist_1_0_0_0(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_COMMA);
        result_ = result_ && formlist_1_0_0_2(builder_, level_ + 1);
        result_ = result_ && form(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // EOL?
    private static boolean formlist_1_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_1_0_0_0")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }

    // EOL?
    private static boolean formlist_1_0_0_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_1_0_0_2")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }


    /* ********************************************************** */
    // FUNCTION '(' formlist ')' expr_or_assign
    public static boolean fundef(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_FUNCTION);
        result_ = result_ && consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && formlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_FUNDEF);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // '(' expr ')'
    public static boolean ifcond(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "ifcond")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (result_) {
            marker_.done(R_IFCOND);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // EOL
    //     |  expr_or_assign? EOL
    //     |  expr_or_assign ';'
    //     |  COMMENT
    public static boolean prog(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "prog")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_EOL);
        if (!result_) result_ = prog_1(builder_, level_ + 1);
        if (!result_) result_ = prog_2(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_COMMENT);
        if (result_) {
            marker_.done(R_PROG);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // expr_or_assign? EOL
    private static boolean prog_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "prog_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = prog_1_0(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_EOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // expr_or_assign?
    private static boolean prog_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "prog_1_0")) return false;
        expr_or_assign(builder_, level_ + 1);
        return true;
    }

    // expr_or_assign ';'
    private static boolean prog_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "prog_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_or_assign(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_SEMICOLON);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // SYMBOL '=' expr
    // //   | SYMBOL
    //    | STR_CONST '=' expr
    // //   | STR_CONST
    //    | NULL_CONST '=' expr
    // //   | NULL_CONST
    //    | expr
    public static boolean sub(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sub_0(builder_, level_ + 1);
        if (!result_) result_ = sub_1(builder_, level_ + 1);
        if (!result_) result_ = sub_2(builder_, level_ + 1);
        if (!result_) result_ = expr(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_SUB);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // SYMBOL '=' expr
    private static boolean sub_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // STR_CONST '=' expr
    private static boolean sub_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // NULL_CONST '=' expr
    private static boolean sub_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NULL_CONST);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // sub (EOL? ',' sub)*
    public static boolean sublist(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sub(builder_, level_ + 1);
        result_ = result_ && sublist_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_SUBLIST);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // (EOL? ',' sub)*
    private static boolean sublist_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!sublist_1_0(builder_, level_ + 1)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "sublist_1");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }

    // (EOL? ',' sub)
    private static boolean sublist_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1_0")) return false;
        return sublist_1_0_0(builder_, level_ + 1);
    }

    // EOL? ',' sub
    private static boolean sublist_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sublist_1_0_0_0(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_COMMA);
        result_ = result_ && sub(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // EOL?
    private static boolean sublist_1_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1_0_0_0")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }


    final static Parser prog_parser_ = new Parser() {
        public boolean parse(PsiBuilder builder_, int level_) {
            return prog(builder_, level_ + 1);
        }
    };
}
