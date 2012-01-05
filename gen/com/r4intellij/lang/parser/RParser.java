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
        } else if (root_ == R_CR) {
            result_ = cr(builder_, level_ + 1);
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
        } else if (root_ == R_FORMLIST) {
            result_ = formlist(builder_, level_ + 1);
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
    public static boolean cr(PsiBuilder builder_, int level_) {
        return true;
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
    // '{' exprlist '}'
    //     | '(' expr_or_assign ')'
    //     | '-' expr
    //     | '+' expr
    //     | '!' expr
    //     | '~' expr
    //     | '?' expr
    //     | expr { ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE |
    //      GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN } expr
    //     | FUNCTION '(' formlist ')' cr expr_or_assign
    //     | expr '(' sublist ')'
    //     | IF ifcond expr_or_assign
    //     | IF ifcond expr_or_assign ELSE expr_or_assign
    //     | FOR forcond expr_or_assign
    //     | WHILE cond expr_or_assign
    //     | REPEAT expr_or_assign
    //     | expr LBB sublist RBB
    //     | expr '[' sublist ']'
    //     | SYMBOL NS_GET SYMBOL
    //     | SYMBOL NS_GET STR_CONST
    //     | STR_CONST NS_GET SYMBOL
    //     | STR_CONST NS_GET STR_CONST
    //     | SYMBOL NS_GET_INT SYMBOL
    //     | SYMBOL NS_GET_INT STR_CONST
    //     | STR_CONST NS_GET_INT SYMBOL
    //     | STR_CONST NS_GET_INT STR_CONST
    //     | expr '$' SYMBOL
    //     | expr '$' STR_CONST
    //     | expr '@' SYMBOL
    //     | expr '@' STR_CONST
    //      | NUM_CONST
    //         | STR_CONST
    //         | NULL_CONST
    //         | SYMBOL
    //     | NEXT
    //     | BREAK
    public static boolean expr(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_0(builder_, level_ + 1);
        if (!result_) result_ = expr_1(builder_, level_ + 1);
        if (!result_) result_ = expr_2(builder_, level_ + 1);
        if (!result_) result_ = expr_3(builder_, level_ + 1);
        if (!result_) result_ = expr_4(builder_, level_ + 1);
        if (!result_) result_ = expr_5(builder_, level_ + 1);
        if (!result_) result_ = expr_6(builder_, level_ + 1);
        if (!result_) result_ = expr_7(builder_, level_ + 1);
        if (!result_) result_ = expr_8(builder_, level_ + 1);
        if (!result_) result_ = expr_9(builder_, level_ + 1);
        if (!result_) result_ = expr_10(builder_, level_ + 1);
        if (!result_) result_ = expr_11(builder_, level_ + 1);
        if (!result_) result_ = expr_12(builder_, level_ + 1);
        if (!result_) result_ = expr_13(builder_, level_ + 1);
        if (!result_) result_ = expr_14(builder_, level_ + 1);
        if (!result_) result_ = expr_15(builder_, level_ + 1);
        if (!result_) result_ = expr_16(builder_, level_ + 1);
        if (!result_) result_ = expr_17(builder_, level_ + 1);
        if (!result_) result_ = expr_18(builder_, level_ + 1);
        if (!result_) result_ = expr_19(builder_, level_ + 1);
        if (!result_) result_ = expr_20(builder_, level_ + 1);
        if (!result_) result_ = expr_21(builder_, level_ + 1);
        if (!result_) result_ = expr_22(builder_, level_ + 1);
        if (!result_) result_ = expr_23(builder_, level_ + 1);
        if (!result_) result_ = expr_24(builder_, level_ + 1);
        if (!result_) result_ = expr_25(builder_, level_ + 1);
        if (!result_) result_ = expr_26(builder_, level_ + 1);
        if (!result_) result_ = expr_27(builder_, level_ + 1);
        if (!result_) result_ = expr_28(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_NUM_CONST);
        if (!result_) result_ = consumeToken(builder_, R_STR_CONST);
        if (!result_) result_ = consumeToken(builder_, R_NULL_CONST);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL);
        if (!result_) result_ = consumeToken(builder_, R_NEXT);
        if (!result_) result_ = consumeToken(builder_, R_BREAK);
        if (result_) {
            marker_.done(R_EXPR);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // '{' exprlist '}'
    private static boolean expr_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0")) return false;
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
    private static boolean expr_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1")) return false;
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
    private static boolean expr_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2")) return false;
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
    private static boolean expr_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_3")) return false;
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
    private static boolean expr_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_4")) return false;
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
    private static boolean expr_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_5")) return false;
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
    private static boolean expr_6(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_6")) return false;
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

    // expr { ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE |
    //      GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN } expr
    private static boolean expr_7(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_7")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && expr_7_1(builder_, level_ + 1);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // { ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE |
    //      GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN }
    private static boolean expr_7_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_7_1")) return false;
        return expr_7_1_0(builder_, level_ + 1);
    }

    // ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE |
    //      GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN
    private static boolean expr_7_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_7_1_0")) return false;
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

    // FUNCTION '(' formlist ')' cr expr_or_assign
    private static boolean expr_8(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_8")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_FUNCTION);
        result_ = result_ && consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && formlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        result_ = result_ && cr(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // expr '(' sublist ')'
    private static boolean expr_9(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_9")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && sublist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // IF ifcond expr_or_assign
    private static boolean expr_10(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_10")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_IF);
        result_ = result_ && ifcond(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // IF ifcond expr_or_assign ELSE expr_or_assign
    private static boolean expr_11(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_11")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_IF);
        result_ = result_ && ifcond(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_ELSE);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // FOR forcond expr_or_assign
    private static boolean expr_12(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_12")) return false;
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
    private static boolean expr_13(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_13")) return false;
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
    private static boolean expr_14(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_14")) return false;
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

    // expr LBB sublist RBB
    private static boolean expr_15(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_15")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LBB);
        result_ = result_ && sublist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RBB);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // expr '[' sublist ']'
    private static boolean expr_16(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_16")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LEFT_BRACKET);
        result_ = result_ && sublist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_BRACKET);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // SYMBOL NS_GET SYMBOL
    private static boolean expr_17(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_17")) return false;
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
    private static boolean expr_18(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_18")) return false;
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
    private static boolean expr_19(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_19")) return false;
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
    private static boolean expr_20(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_20")) return false;
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
    private static boolean expr_21(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_21")) return false;
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
    private static boolean expr_22(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_22")) return false;
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
    private static boolean expr_23(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_23")) return false;
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
    private static boolean expr_24(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_24")) return false;
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

    // expr '$' SYMBOL
    private static boolean expr_25(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_25")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LIST_SUBSET);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // expr '$' STR_CONST
    private static boolean expr_26(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_26")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LIST_SUBSET);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // expr '@' SYMBOL
    private static boolean expr_27(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_27")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_SLOT);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // expr '@' STR_CONST
    private static boolean expr_28(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_28")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_SLOT);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // expr | equal_assign
    public static boolean expr_or_assign(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        if (!result_) result_ = equal_assign(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPR_OR_ASSIGN);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // whitespace
    //         | expr_or_assign
    //         | exprlist ';' expr_or_assign
    //         | exprlist ';'
    //         | exprlist '\n' expr_or_assign
    //         | exprlist '\n'
    public static boolean exprlist(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_WHITESPACE);
        if (!result_) result_ = expr_or_assign(builder_, level_ + 1);
        if (!result_) result_ = exprlist_2(builder_, level_ + 1);
        if (!result_) result_ = exprlist_3(builder_, level_ + 1);
        if (!result_) result_ = exprlist_4(builder_, level_ + 1);
        if (!result_) result_ = exprlist_5(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPRLIST);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // exprlist ';' expr_or_assign
    private static boolean exprlist_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_SEMICOLON);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // exprlist ';'
    private static boolean exprlist_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_3")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_SEMICOLON);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // exprlist '\n' expr_or_assign
    private static boolean exprlist_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_4")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LINE_BREAK);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // exprlist '\n'
    private static boolean exprlist_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_5")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LINE_BREAK);
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
    // whitespace | SYMBOL
    //         | SYMBOL EQ_ASSIGN expr
    //         | formlist ',' SYMBOL
    //         | formlist ',' SYMBOL EQ_ASSIGN expr
    //         | SYMBOL_FORMALS
    public static boolean formlist(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_WHITESPACE);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL);
        if (!result_) result_ = formlist_2(builder_, level_ + 1);
        if (!result_) result_ = formlist_3(builder_, level_ + 1);
        if (!result_) result_ = formlist_4(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL_FORMALS);
        if (result_) {
            marker_.done(R_FORMLIST);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // SYMBOL EQ_ASSIGN expr
    private static boolean formlist_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_2")) return false;
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

    // formlist ',' SYMBOL
    private static boolean formlist_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_3")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = formlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_COMMA);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // formlist ',' SYMBOL EQ_ASSIGN expr
    private static boolean formlist_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "formlist_4")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = formlist(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_COMMA);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
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
    // END_OF_INPUT //todo can not work
    //     | '\n'
    //     | expr_or_assign '\n'
    //     | expr_or_assign ';'
    //     | COMMENT
    public static boolean prog(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "prog")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_END_OF_INPUT);
        if (!result_) result_ = consumeToken(builder_, R_LINE_BREAK);
        if (!result_) result_ = prog_2(builder_, level_ + 1);
        if (!result_) result_ = prog_3(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_COMMENT);
        if (result_) {
            marker_.done(R_PROG);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // expr_or_assign '\n'
    private static boolean prog_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "prog_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_or_assign(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LINE_BREAK);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // expr_or_assign ';'
    private static boolean prog_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "prog_3")) return false;
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
    // whitespace | expr
    //    | SYMBOL EQ_ASSIGN
    //    | SYMBOL EQ_ASSIGN expr
    //    | STR_CONST EQ_ASSIGN
    //    | STR_CONST EQ_ASSIGN expr
    //    | NULL_CONST EQ_ASSIGN
    //    | NULL_CONST EQ_ASSIGN expr
    public static boolean sub(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_WHITESPACE);
        if (!result_) result_ = expr(builder_, level_ + 1);
        if (!result_) result_ = sub_2(builder_, level_ + 1);
        if (!result_) result_ = sub_3(builder_, level_ + 1);
        if (!result_) result_ = sub_4(builder_, level_ + 1);
        if (!result_) result_ = sub_5(builder_, level_ + 1);
        if (!result_) result_ = sub_6(builder_, level_ + 1);
        if (!result_) result_ = sub_7(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_SUB);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // SYMBOL EQ_ASSIGN
    private static boolean sub_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // SYMBOL EQ_ASSIGN expr
    private static boolean sub_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_3")) return false;
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

    // STR_CONST EQ_ASSIGN
    private static boolean sub_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_4")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // STR_CONST EQ_ASSIGN expr
    private static boolean sub_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_5")) return false;
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

    // NULL_CONST EQ_ASSIGN
    private static boolean sub_6(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_6")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NULL_CONST);
        result_ = result_ && consumeToken(builder_, R_EQ_ASSIGN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }

    // NULL_CONST EQ_ASSIGN expr
    private static boolean sub_7(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_7")) return false;
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
    // sub | sublist cr ',' sub
    public static boolean sublist(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sub(builder_, level_ + 1);
        if (!result_) result_ = sublist_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_SUBLIST);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }

    // sublist cr ',' sub
    private static boolean sublist_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sublist(builder_, level_ + 1);
        result_ = result_ && cr(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_COMMA);
        result_ = result_ && sub(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    final static Parser prog_parser_ = new Parser() {
        public boolean parse(PsiBuilder builder_, int level_) {
            return prog(builder_, level_ + 1);
        }
    };
}
