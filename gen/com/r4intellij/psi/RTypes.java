/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.r4intellij.lang.lexer.RTokenType;
import com.r4intellij.psi.impl.*;


public interface RTypes {

    IElementType R_COND = new RCompositeElementType("R_COND");
    IElementType R_EQUAL_ASSIGN = new RCompositeElementType("R_EQUAL_ASSIGN");
    IElementType R_EXPR = new RCompositeElementType("R_EXPR");
    IElementType R_EXPR_OR_ASSIGN = new RCompositeElementType("R_EXPR_OR_ASSIGN");
    IElementType R_EXPRLIST = new RCompositeElementType("R_EXPRLIST");
    IElementType R_FORCOND = new RCompositeElementType("R_FORCOND");
    IElementType R_FORM = new RCompositeElementType("R_FORM");
    IElementType R_FORMLIST = new RCompositeElementType("R_FORMLIST");
    IElementType R_FUNDEF = new RCompositeElementType("R_FUNDEF");
    IElementType R_IFCOND = new RCompositeElementType("R_IFCOND");
    IElementType R_PROG = new RCompositeElementType("R_PROG");
    IElementType R_SUB = new RCompositeElementType("R_SUB");
    IElementType R_SUBLIST = new RCompositeElementType("R_SUBLIST");
    IElementType R_VARIABLE = new RCompositeElementType("R_VARIABLE");

    IElementType R_AND = new RTokenType("&");
    IElementType R_AND2 = new RTokenType("&&");
    IElementType R_ARITH_DIV = new RTokenType("/");
    IElementType R_ARITH_EXPONENTIAION = new RTokenType("^");
    IElementType R_ARITH_MINUS = new RTokenType("-");
    IElementType R_ARITH_MISC = new RTokenType("ARITH_MISC");
    IElementType R_ARITH_MOD = new RTokenType("%");
    IElementType R_ARITH_MULT = new RTokenType("*");
    IElementType R_ARITH_PLUS = new RTokenType("+");
    IElementType R_BREAK = new RTokenType("BREAK");
    IElementType R_COLON = new RTokenType(":");
    IElementType R_COMMA = new RTokenType(",");
    IElementType R_COMMENT = new RTokenType("COMMENT");
    IElementType R_ELSE = new RTokenType("ELSE");
    IElementType R_EOL = new RTokenType("EOL");
    IElementType R_EQ = new RTokenType("==");
    IElementType R_EQ_ASSIGN = new RTokenType("=");
    IElementType R_FOR = new RTokenType("FOR");
    IElementType R_FUNCTION = new RTokenType("FUNCTION");
    IElementType R_GE = new RTokenType(">=");
    IElementType R_GT = new RTokenType(">");
    IElementType R_IF = new RTokenType("IF");
    IElementType R_IN = new RTokenType("IN");
    IElementType R_LBB = new RTokenType("[[");
    IElementType R_LE = new RTokenType("<=");
    IElementType R_LEFT_ASSIGN = new RTokenType("<-");
    IElementType R_LEFT_BRACE = new RTokenType("{");
    IElementType R_LEFT_BRACKET = new RTokenType("[");
    IElementType R_LEFT_PAREN = new RTokenType("(");
    IElementType R_LIST_SUBSET = new RTokenType("$");
    IElementType R_LT = new RTokenType("<");
    IElementType R_NE = new RTokenType("!=");
    IElementType R_NEGATION = new RTokenType("!");
    IElementType R_NEXT = new RTokenType("NEXT");
    IElementType R_NS_GET = new RTokenType("::");
    IElementType R_NS_GET_INT = new RTokenType(":::");
    IElementType R_NULL_CONST = new RTokenType("NULL_CONST");
    IElementType R_NUM_CONST = new RTokenType("NUM_CONST");
    IElementType R_OR = new RTokenType("|");
    IElementType R_OR2 = new RTokenType("||");
    IElementType R_QUESTION = new RTokenType("?");
    IElementType R_RBB = new RTokenType("]]");
    IElementType R_REPEAT = new RTokenType("REPEAT");
    IElementType R_RIGHT_ASSIGN = new RTokenType("->");
    IElementType R_RIGHT_BRACE = new RTokenType("}");
    IElementType R_RIGHT_BRACKET = new RTokenType("]");
    IElementType R_RIGHT_PAREN = new RTokenType(")");
    IElementType R_SEMICOLON = new RTokenType(";");
    IElementType R_SLOT = new RTokenType("@");
    IElementType R_STR_CONST = new RTokenType("STR_CONST");
    IElementType R_SYMBOL = new RTokenType("SYMBOL");
    IElementType R_SYMBOL_FORMALS = new RTokenType("...");
    IElementType R_TILDE = new RTokenType("~");
    IElementType R_WHILE = new RTokenType("WHILE");

    class Factory {

        public static PsiElement createElement(ASTNode node) {
            IElementType type = node.getElementType();
            if (type == R_COND) {
                return new RCondImpl(node);
            } else if (type == R_EQUAL_ASSIGN) {
                return new REqualAssignImpl(node);
            } else if (type == R_EXPR) {
                return new RExprImpl(node);
            } else if (type == R_EXPR_OR_ASSIGN) {
                return new RExprOrAssignImpl(node);
            } else if (type == R_EXPRLIST) {
                return new RExprlistImpl(node);
            } else if (type == R_FORCOND) {
                return new RForcondImpl(node);
            } else if (type == R_FORM) {
                return new RFormImpl(node);
            } else if (type == R_FORMLIST) {
                return new RFormlistImpl(node);
            } else if (type == R_FUNDEF) {
                return new RFundefImpl(node);
            } else if (type == R_IFCOND) {
                return new RIfcondImpl(node);
            } else if (type == R_PROG) {
                return new RProgImpl(node);
            } else if (type == R_SUB) {
                return new RSubImpl(node);
            } else if (type == R_SUBLIST) {
                return new RSublistImpl(node);
            } else if (type == R_VARIABLE) {
                return new RVariableImpl(node);
            }
            throw new AssertionError("Unknown element type: " + type);
        }
    }
}
