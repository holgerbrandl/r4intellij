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
        if (root_ == R_COMMAND) {
            result_ = command(builder_, level_ + 1);
        } else if (root_ == R_COND) {
            result_ = cond(builder_, level_ + 1);
        } else if (root_ == R_EQUAL_ASSIGN) {
            result_ = equal_assign(builder_, level_ + 1);
        } else if (root_ == R_EXPR) {
            result_ = expr(builder_, level_ + 1);
        } else if (root_ == R_EXPR_OR_ASSIGN) {
            result_ = expr_or_assign(builder_, level_ + 1);
        } else if (root_ == R_EXPRLIST) {
            result_ = exprlist(builder_, level_ + 1);
        } else if (root_ == R_FD_ARGUMENT) {
            result_ = fd_argument(builder_, level_ + 1);
        } else if (root_ == R_FORCOND) {
            result_ = forcond(builder_, level_ + 1);
        } else if (root_ == R_FUNCALL) {
            result_ = funcall(builder_, level_ + 1);
        } else if (root_ == R_FUNDEF) {
            result_ = fundef(builder_, level_ + 1);
        } else if (root_ == R_FUNDEF_ARGS) {
            result_ = fundef_args(builder_, level_ + 1);
        } else if (root_ == R_SECTION) {
            result_ = section(builder_, level_ + 1);
        } else if (root_ == R_STRING_LITERAL) {
            result_ = string_literal(builder_, level_ + 1);
        } else if (root_ == R_SUB) {
            result_ = sub(builder_, level_ + 1);
        } else if (root_ == R_SUBLIST) {
            result_ = sublist(builder_, level_ + 1);
        } else if (root_ == R_VARIABLE) {
            result_ = variable(builder_, level_ + 1);
        } else {
            Marker marker_ = builder_.mark();
            result_ = parseGrammar(builder_, level_ + 1, command_parser_);
            while (builder_.getTokenType() != null) {
                builder_.advanceLexer();
            }
            marker_.done(root_);
        }
        return builder_.getTreeBuilt();
    }


    /* ********************************************************** */
    // section
    //     |  expr_or_assign? (EOL | ';')
    public static boolean command(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "command")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        enterErrorRecordingSection(builder_, level_, _SECTION_RECOVER_);
        result_ = section(builder_, level_ + 1);
        if (!result_) result_ = command_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_COMMAND);
        } else {
            marker_.rollbackTo();
        }
        result_ = exitErrorRecordingSection(builder_, result_, level_, false, _SECTION_RECOVER_, command_recover_until_parser_);
        return result_;
    }


    // expr_or_assign? (EOL | ';')
    private static boolean command_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "command_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = command_1_0(builder_, level_ + 1);
        result_ = result_ && command_1_1(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // expr_or_assign?
    private static boolean command_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "command_1_0")) return false;
        expr_or_assign(builder_, level_ + 1);
        return true;
    }


    // (EOL | ';')
    private static boolean command_1_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "command_1_1")) return false;
        return command_1_1_0(builder_, level_ + 1);
    }


    // EOL | ';'
    private static boolean command_1_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "command_1_1_0")) return false;
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
    // !(command)
    static boolean command_recover_until(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "command_recover_until")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        enterErrorRecordingSection(builder_, level_, _SECTION_NOT_);
        result_ = !command_recover_until_0(builder_, level_ + 1);
        marker_.rollbackTo();
        result_ = exitErrorRecordingSection(builder_, result_, level_, false, _SECTION_NOT_, null);
        return result_;
    }


    // (command)
    private static boolean command_recover_until_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "command_recover_until_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = command(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // '(' expr ')'
    public static boolean cond(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "cond")) return false;
        if (!nextTokenIs(builder_, R_LEFT_PAREN)) return false;
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
    // EOL* (
    //     NUM_CONST |
    //     NULL_CONST |
    // //    string_literal (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST) |
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
    //     (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    //     '(' sublist? ')' |
    //     '[[' sublist ']]' |
    //     '[' ','? sublist ','? ']' |
    //     '$' ( SYMBOL | STR_CONST) |
    //     '@' ( SYMBOL | STR_CONST )
    //     )*
    public static boolean expr(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_0(builder_, level_ + 1);
        result_ = result_ && expr_1(builder_, level_ + 1);
        result_ = result_ && expr_2(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPR);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // EOL*
    private static boolean expr_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_0")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!consumeToken(builder_, R_EOL)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "expr_0");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    // (
    //     NUM_CONST |
    //     NULL_CONST |
    // //    string_literal (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST) |
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
    //     BREAK)
    private static boolean expr_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1")) return false;
        return expr_1_0(builder_, level_ + 1);
    }


    // NUM_CONST |
    //     NULL_CONST |
    // //    string_literal (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST) |
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
    private static boolean expr_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NUM_CONST);
        if (!result_) result_ = consumeToken(builder_, R_NULL_CONST);
        if (!result_) result_ = string_literal(builder_, level_ + 1);
        if (!result_) result_ = funcall(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_4(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_5(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_6(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_7(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_8(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_9(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_10(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_11(builder_, level_ + 1);
        if (!result_) result_ = fundef(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_13(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_14(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_15(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_16(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_NEXT);
        if (!result_) result_ = consumeToken(builder_, R_BREAK);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // variable (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST)?
    private static boolean expr_1_0_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = variable(builder_, level_ + 1);
        result_ = result_ && expr_1_0_4_1(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST)?
    private static boolean expr_1_0_4_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4_1")) return false;
        expr_1_0_4_1_0(builder_, level_ + 1);
        return true;
    }


    // (NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST)
    private static boolean expr_1_0_4_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4_1_0")) return false;
        return expr_1_0_4_1_0_0(builder_, level_ + 1);
    }


    // NS_GET SYMBOL | NS_GET STR_CONST | NS_GET_INT SYMBOL | NS_GET_INT STR_CONST
    private static boolean expr_1_0_4_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_1_0_4_1_0_0_0(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_4_1_0_0_1(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_4_1_0_0_2(builder_, level_ + 1);
        if (!result_) result_ = expr_1_0_4_1_0_0_3(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // NS_GET SYMBOL
    private static boolean expr_1_0_4_1_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4_1_0_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NS_GET);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // NS_GET STR_CONST
    private static boolean expr_1_0_4_1_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4_1_0_0_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NS_GET);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // NS_GET_INT SYMBOL
    private static boolean expr_1_0_4_1_0_0_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4_1_0_0_2")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NS_GET_INT);
        result_ = result_ && consumeToken(builder_, R_SYMBOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // NS_GET_INT STR_CONST
    private static boolean expr_1_0_4_1_0_0_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_4_1_0_0_3")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_NS_GET_INT);
        result_ = result_ && consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // '{' exprlist '}'
    private static boolean expr_1_0_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_5")) return false;
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
    private static boolean expr_1_0_6(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_6")) return false;
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
    private static boolean expr_1_0_7(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_7")) return false;
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
    private static boolean expr_1_0_8(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_8")) return false;
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
    private static boolean expr_1_0_9(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_9")) return false;
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
    private static boolean expr_1_0_10(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_10")) return false;
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
    private static boolean expr_1_0_11(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_11")) return false;
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


    // IF cond expr_or_assign [ELSE expr_or_assign]
    private static boolean expr_1_0_13(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_13")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_IF);
        result_ = result_ && cond(builder_, level_ + 1);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        result_ = result_ && expr_1_0_13_3(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // [ELSE expr_or_assign]
    private static boolean expr_1_0_13_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_13_3")) return false;
        expr_1_0_13_3_0(builder_, level_ + 1);
        return true;
    }


    // ELSE expr_or_assign
    private static boolean expr_1_0_13_3_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_13_3_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_ELSE);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // FOR forcond expr_or_assign
    private static boolean expr_1_0_14(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_14")) return false;
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
    private static boolean expr_1_0_15(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_15")) return false;
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
    private static boolean expr_1_0_16(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_1_0_16")) return false;
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


    // (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    //     '(' sublist? ')' |
    //     '[[' sublist ']]' |
    //     '[' ','? sublist ','? ']' |
    //     '$' ( SYMBOL | STR_CONST) |
    //     '@' ( SYMBOL | STR_CONST )
    //     )*
    private static boolean expr_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!expr_2_0(builder_, level_ + 1)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "expr_2");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    // (( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    //     '(' sublist? ')' |
    //     '[[' sublist ']]' |
    //     '[' ','? sublist ','? ']' |
    //     '$' ( SYMBOL | STR_CONST) |
    //     '@' ( SYMBOL | STR_CONST )
    //     )
    private static boolean expr_2_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0")) return false;
        return expr_2_0_0(builder_, level_ + 1);
    }


    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr |
    //     '(' sublist? ')' |
    //     '[[' sublist ']]' |
    //     '[' ','? sublist ','? ']' |
    //     '$' ( SYMBOL | STR_CONST) |
    //     '@' ( SYMBOL | STR_CONST )
    private static boolean expr_2_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_2_0_0_0(builder_, level_ + 1);
        if (!result_) result_ = expr_2_0_0_1(builder_, level_ + 1);
        if (!result_) result_ = expr_2_0_0_2(builder_, level_ + 1);
        if (!result_) result_ = expr_2_0_0_3(builder_, level_ + 1);
        if (!result_) result_ = expr_2_0_0_4(builder_, level_ + 1);
        if (!result_) result_ = expr_2_0_0_5(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN ) expr
    private static boolean expr_2_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_2_0_0_0_0(builder_, level_ + 1);
        result_ = result_ && expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // ( ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN )
    private static boolean expr_2_0_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_0_0")) return false;
        return expr_2_0_0_0_0_0(builder_, level_ + 1);
    }


    // ':' | '+' | '-' | '*' | '/' | '^' | ARITH_MISC | '%' | '~' | '?' | LT | LE | EQ | NE | GE | GT | AND | OR | AND2 | OR2 | LEFT_ASSIGN | RIGHT_ASSIGN
    private static boolean expr_2_0_0_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_0_0_0")) return false;
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


    // '(' sublist? ')'
    private static boolean expr_2_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_1")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && expr_2_0_0_1_1(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // sublist?
    private static boolean expr_2_0_0_1_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_1_1")) return false;
        sublist(builder_, level_ + 1);
        return true;
    }


    // '[[' sublist ']]'
    private static boolean expr_2_0_0_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_2")) return false;
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
    private static boolean expr_2_0_0_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_3")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_BRACKET);
        result_ = result_ && expr_2_0_0_3_1(builder_, level_ + 1);
        result_ = result_ && sublist(builder_, level_ + 1);
        result_ = result_ && expr_2_0_0_3_3(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_BRACKET);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // ','?
    private static boolean expr_2_0_0_3_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_3_1")) return false;
        consumeToken(builder_, R_COMMA);
        return true;
    }


    // ','?
    private static boolean expr_2_0_0_3_3(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_3_3")) return false;
        consumeToken(builder_, R_COMMA);
        return true;
    }


    // '$' ( SYMBOL | STR_CONST)
    private static boolean expr_2_0_0_4(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_4")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LIST_SUBSET);
        result_ = result_ && expr_2_0_0_4_1(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // ( SYMBOL | STR_CONST)
    private static boolean expr_2_0_0_4_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_4_1")) return false;
        return expr_2_0_0_4_1_0(builder_, level_ + 1);
    }


    // SYMBOL | STR_CONST
    private static boolean expr_2_0_0_4_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_4_1_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        if (!result_) result_ = consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // '@' ( SYMBOL | STR_CONST )
    private static boolean expr_2_0_0_5(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_5")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SLOT);
        result_ = result_ && expr_2_0_0_5_1(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // ( SYMBOL | STR_CONST )
    private static boolean expr_2_0_0_5_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_5_1")) return false;
        return expr_2_0_0_5_1_0(builder_, level_ + 1);
    }


    // SYMBOL | STR_CONST
    private static boolean expr_2_0_0_5_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_2_0_0_5_1_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        if (!result_) result_ = consumeToken(builder_, R_STR_CONST);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // EOL* ( expr [EQ_ASSIGN expr_or_assign] |  COMMENT )
    public static boolean expr_or_assign(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_or_assign_0(builder_, level_ + 1);
        result_ = result_ && expr_or_assign_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPR_OR_ASSIGN);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // EOL*
    private static boolean expr_or_assign_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_0")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!consumeToken(builder_, R_EOL)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "expr_or_assign_0");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    // ( expr [EQ_ASSIGN expr_or_assign] |  COMMENT )
    private static boolean expr_or_assign_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_1")) return false;
        return expr_or_assign_1_0(builder_, level_ + 1);
    }


    // expr [EQ_ASSIGN expr_or_assign] |  COMMENT
    private static boolean expr_or_assign_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_1_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_or_assign_1_0_0(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_COMMENT);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // expr [EQ_ASSIGN expr_or_assign]
    private static boolean expr_or_assign_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr(builder_, level_ + 1);
        result_ = result_ && expr_or_assign_1_0_0_1(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // [EQ_ASSIGN expr_or_assign]
    private static boolean expr_or_assign_1_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_1_0_0_1")) return false;
        expr_or_assign_1_0_0_1_0(builder_, level_ + 1);
        return true;
    }


    // EQ_ASSIGN expr_or_assign
    private static boolean expr_or_assign_1_0_0_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "expr_or_assign_1_0_0_1_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_EQ_ASSIGN);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    /* ********************************************************** */
    // [(expr_or_assign) (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*] EOL*
    public static boolean exprlist(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist_0(builder_, level_ + 1);
        result_ = result_ && exprlist_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_EXPRLIST);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // [(expr_or_assign) (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*]
    private static boolean exprlist_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0")) return false;
        exprlist_0_0(builder_, level_ + 1);
        return true;
    }


    // (expr_or_assign) (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*
    private static boolean exprlist_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist_0_0_0(builder_, level_ + 1);
        result_ = result_ && exprlist_0_0_1(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // (expr_or_assign)
    private static boolean exprlist_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = expr_or_assign(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)*
    private static boolean exprlist_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0_0_1")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!exprlist_0_0_1_0(builder_, level_ + 1)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "exprlist_0_0_1");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    // (';' expr_or_assign | EOL expr_or_assign |';' |  EOL)
    private static boolean exprlist_0_0_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0_0_1_0")) return false;
        return exprlist_0_0_1_0_0(builder_, level_ + 1);
    }


    // ';' expr_or_assign | EOL expr_or_assign |';' |  EOL
    private static boolean exprlist_0_0_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0_0_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = exprlist_0_0_1_0_0_0(builder_, level_ + 1);
        if (!result_) result_ = exprlist_0_0_1_0_0_1(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_SEMICOLON);
        if (!result_) result_ = consumeToken(builder_, R_EOL);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // ';' expr_or_assign
    private static boolean exprlist_0_0_1_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0_0_1_0_0_0")) return false;
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
    private static boolean exprlist_0_0_1_0_0_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_0_0_1_0_0_1")) return false;
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


    // EOL*
    private static boolean exprlist_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "exprlist_1")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!consumeToken(builder_, R_EOL)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "exprlist_1");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    /* ********************************************************** */
    // SYMBOL '=' expr
    //         | SYMBOL
    //         | STR_CONST '=' expr
    //         | STR_CONST
    //         | SYMBOL_FORMALS
    public static boolean fd_argument(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fd_argument")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = fd_argument_0(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL);
        if (!result_) result_ = fd_argument_2(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_STR_CONST);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL_FORMALS);
        if (result_) {
            marker_.done(R_FD_ARGUMENT);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // SYMBOL '=' expr
    private static boolean fd_argument_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fd_argument_0")) return false;
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
    private static boolean fd_argument_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fd_argument_2")) return false;
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
    // '(' variable IN expr ')'
    public static boolean forcond(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "forcond")) return false;
        if (!nextTokenIs(builder_, R_LEFT_PAREN)) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && variable(builder_, level_ + 1);
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
    // variable '(' sublist? ')'
    public static boolean funcall(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "funcall")) return false;
        if (!nextTokenIs(builder_, R_SYMBOL)) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = variable(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && funcall_2(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        if (result_) {
            marker_.done(R_FUNCALL);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // sublist?
    private static boolean funcall_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "funcall_2")) return false;
        sublist(builder_, level_ + 1);
        return true;
    }


    /* ********************************************************** */
    // FUNCTION '(' fundef_args? ')'  expr_or_assign
    public static boolean fundef(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef")) return false;
        if (!nextTokenIs(builder_, R_FUNCTION)) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_FUNCTION);
        result_ = result_ && consumeToken(builder_, R_LEFT_PAREN);
        result_ = result_ && fundef_2(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_RIGHT_PAREN);
        result_ = result_ && expr_or_assign(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_FUNDEF);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // fundef_args?
    private static boolean fundef_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef_2")) return false;
        fundef_args(builder_, level_ + 1);
        return true;
    }


    /* ********************************************************** */
    // fd_argument (EOL? ',' EOL? fd_argument)*
    public static boolean fundef_args(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef_args")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = fd_argument(builder_, level_ + 1);
        result_ = result_ && fundef_args_1(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_FUNDEF_ARGS);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // (EOL? ',' EOL? fd_argument)*
    private static boolean fundef_args_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef_args_1")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!fundef_args_1_0(builder_, level_ + 1)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "fundef_args_1");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    // (EOL? ',' EOL? fd_argument)
    private static boolean fundef_args_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef_args_1_0")) return false;
        return fundef_args_1_0_0(builder_, level_ + 1);
    }


    // EOL? ',' EOL? fd_argument
    private static boolean fundef_args_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef_args_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = fundef_args_1_0_0_0(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_COMMA);
        result_ = result_ && fundef_args_1_0_0_2(builder_, level_ + 1);
        result_ = result_ && fd_argument(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // EOL?
    private static boolean fundef_args_1_0_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef_args_1_0_0_0")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }


    // EOL?
    private static boolean fundef_args_1_0_0_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "fundef_args_1_0_0_2")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }


    /* ********************************************************** */
    // SECTION_COMMENT
    public static boolean section(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "section")) return false;
        if (!nextTokenIs(builder_, R_SECTION_COMMENT)) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SECTION_COMMENT);
        if (result_) {
            marker_.done(R_SECTION);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // STR_CONST
    public static boolean string_literal(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "string_literal")) return false;
        if (!nextTokenIs(builder_, R_STR_CONST)) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_STR_CONST);
        if (result_) {
            marker_.done(R_STRING_LITERAL);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    /* ********************************************************** */
    // EOL*
    //     (SYMBOL '=' expr
    // //   | STR_CONST '=' expr
    // //   | NULL_CONST '=' expr
    //     | SYMBOL_FORMALS
    //    | expr ) EOL*
    public static boolean sub(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sub_0(builder_, level_ + 1);
        result_ = result_ && sub_1(builder_, level_ + 1);
        result_ = result_ && sub_2(builder_, level_ + 1);
        if (result_) {
            marker_.done(R_SUB);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    // EOL*
    private static boolean sub_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_0")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!consumeToken(builder_, R_EOL)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "sub_0");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    // (SYMBOL '=' expr
    // //   | STR_CONST '=' expr
    // //   | NULL_CONST '=' expr
    //     | SYMBOL_FORMALS
    //    | expr )
    private static boolean sub_1(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_1")) return false;
        return sub_1_0(builder_, level_ + 1);
    }


    // SYMBOL '=' expr
    // //   | STR_CONST '=' expr
    // //   | NULL_CONST '=' expr
    //     | SYMBOL_FORMALS
    //    | expr
    private static boolean sub_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_1_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sub_1_0_0(builder_, level_ + 1);
        if (!result_) result_ = consumeToken(builder_, R_SYMBOL_FORMALS);
        if (!result_) result_ = expr(builder_, level_ + 1);
        if (!result_) {
            marker_.rollbackTo();
        } else {
            marker_.drop();
        }
        return result_;
    }


    // SYMBOL '=' expr
    private static boolean sub_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_1_0_0")) return false;
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


    // EOL*
    private static boolean sub_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sub_2")) return false;
        int offset_ = builder_.getCurrentOffset();
        while (true) {
            if (!consumeToken(builder_, R_EOL)) break;
            int next_offset_ = builder_.getCurrentOffset();
            if (offset_ == next_offset_) {
                empty_element_parsed_guard_(builder_, offset_, "sub_2");
                break;
            }
            offset_ = next_offset_;
        }
        return true;
    }


    /* ********************************************************** */
    // sub (EOL? ',' EOL? sub)*
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


    // (EOL? ',' EOL? sub)*
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


    // (EOL? ',' EOL? sub)
    private static boolean sublist_1_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1_0")) return false;
        return sublist_1_0_0(builder_, level_ + 1);
    }


    // EOL? ',' EOL? sub
    private static boolean sublist_1_0_0(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1_0_0")) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = sublist_1_0_0_0(builder_, level_ + 1);
        result_ = result_ && consumeToken(builder_, R_COMMA);
        result_ = result_ && sublist_1_0_0_2(builder_, level_ + 1);
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


    // EOL?
    private static boolean sublist_1_0_0_2(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "sublist_1_0_0_2")) return false;
        consumeToken(builder_, R_EOL);
        return true;
    }


    /* ********************************************************** */
    // SYMBOL
    public static boolean variable(PsiBuilder builder_, int level_) {
        if (!recursion_guard_(builder_, level_, "variable")) return false;
        if (!nextTokenIs(builder_, R_SYMBOL)) return false;
        boolean result_ = false;
        final Marker marker_ = builder_.mark();
        result_ = consumeToken(builder_, R_SYMBOL);
        if (result_) {
            marker_.done(R_VARIABLE);
        } else {
            marker_.rollbackTo();
        }
        return result_;
    }


    final static Parser command_parser_ = new Parser() {
        public boolean parse(PsiBuilder builder_, int level_) {
            return command(builder_, level_ + 1);
        }
    };
    final static Parser command_recover_until_parser_ = new Parser() {
        public boolean parse(PsiBuilder builder_, int level_) {
            return command_recover_until(builder_, level_ + 1);
        }
    };
}
