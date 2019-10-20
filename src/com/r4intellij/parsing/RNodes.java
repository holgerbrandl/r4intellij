package com.r4intellij.parsing;

import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;

public class RNodes {

    public static final TokenSet EXPR;
    public static final TokenSet STMT;
    public static final TokenSet UNOP;
    public static final TokenSet BINOP;
    public static final TokenSet BLOCK;
    public static final TokenSet CFLOW;
    public static final TokenSet OPEN;
    public static final TokenSet CLOSE;

    public static boolean isExpression(ASTNode node) {
        return is(node, EXPR);
    }

    public static boolean isStatement(ASTNode node) {
        return is(node, STMT);
    }

    public static boolean isUnaryOperator(ASTNode node) {
        return is(node, UNOP);
    }

    public static boolean isBinaryOperator(ASTNode node) {
        return is(node, BINOP);
    }

    public static boolean isBlockParent(ASTNode node) {
        return is(node, BLOCK);
    }

    public static boolean isBlockChild(ASTNode node) {
        return is(node, STMT) && node.getElementType() != RElementTypes.R_BLOCK_EXPRESSION;
    }

    public static boolean isControlFlow(ASTNode node) {
        return is(node, CFLOW);
    }

    private static boolean is(ASTNode node, TokenSet tokenSet) {
        return tokenSet.contains(node.getElementType());
    }

    static {
        TokenSetsBuilder b = new TokenSetsBuilder();
        int literal = b.set();
        int expr = b.setIncluding(literal);
        int stmt = b.setIncluding(expr);
        int cflow = b.set();
        int unop = b.set();
        int assign = b.set();
        int compare = b.set();
        int binop = b.setIncluding(assign, compare);
        int op = b.setIncluding(unop, binop);
        int other = b.set();
        int keyword = b.set();
        int separator = b.set();
        int open = b.set();
        int close = b.set();
        int indentParent = b.set();

        b.classify(RElementTypes.R_ARGUMENT_LIST, other);
        b.classify(RElementTypes.R_ASSIGNMENT_STATEMENT, stmt);
        b.classify(RElementTypes.R_AT_EXPRESSION, expr);
        b.classify(RElementTypes.R_BLOCK_EXPRESSION, expr);
        b.classify(RElementTypes.R_BOOLEAN_LITERAL, literal);
        b.classify(RElementTypes.R_BOUNDARY_LITERAL, literal);
        b.classify(RElementTypes.R_BREAK_STATEMENT, stmt, cflow);
        b.classify(RElementTypes.R_CALL_EXPRESSION, expr, cflow);
        b.classify(RElementTypes.R_EMPTY_EXPRESSION, expr);
        b.classify(RElementTypes.R_EXPRESSION, expr);
        b.classify(RElementTypes.R_FOR_STATEMENT, stmt, cflow, indentParent);
        b.classify(RElementTypes.R_FUNCTION_EXPRESSION, expr);
        b.classify(RElementTypes.R_HELP_EXPRESSION, expr);
        b.classify(RElementTypes.R_IF_STATEMENT, stmt, cflow, indentParent);
        b.classify(RElementTypes.R_MEMBER_EXPRESSION, expr);
        b.classify(RElementTypes.R_NA_LITERAL, literal);
        b.classify(RElementTypes.R_NEXT_STATEMENT, stmt, cflow);
        b.classify(RElementTypes.R_NULL_LITERAL, literal);
        b.classify(RElementTypes.R_NUMERIC_LITERAL_EXPRESSION, literal);
        b.classify(RElementTypes.R_OPERATOR, op);
        b.classify(RElementTypes.R_OPERATOR_EXPRESSION, expr);
        b.classify(RElementTypes.R_PARAMETER, other);
        b.classify(RElementTypes.R_PARAMETER_LIST, other);
        b.classify(RElementTypes.R_PARENTHESIZED_EXPRESSION, expr);
        b.classify(RElementTypes.R_REFERENCE_EXPRESSION, expr);
        b.classify(RElementTypes.R_REPEAT_STATEMENT, stmt, cflow, indentParent);
        b.classify(RElementTypes.R_STRING_LITERAL_EXPRESSION, literal);
        b.classify(RElementTypes.R_SUBSCRIPTION_EXPRESSION, expr);
        b.classify(RElementTypes.R_TILDE_EXPRESSION, expr);
        b.classify(RElementTypes.R_UNARY_TILDE_EXPRESSION, expr);
        b.classify(RElementTypes.R_WHILE_STATEMENT, stmt, cflow, indentParent);

        b.classify(RElementTypes.R_AND, binop);
        b.classify(RElementTypes.R_ANDAND, binop);
        b.classify(RElementTypes.R_AT, binop);
        b.classify(RElementTypes.R_BREAK, keyword, cflow);
        b.classify(RElementTypes.R_COLON, binop);
        b.classify(RElementTypes.R_COMMA, separator);
        b.classify(RElementTypes.R_COMPLEX, literal);
        b.classify(RElementTypes.R_DIV, binop);
        b.classify(RElementTypes.R_DOUBLECOLON, binop);
        b.classify(RElementTypes.R_ELSE, keyword, cflow);
        b.classify(RElementTypes.R_EQ, binop, assign);
        b.classify(RElementTypes.R_EQEQ, binop, compare);
        b.classify(RElementTypes.R_EXP, binop);
        b.classify(RElementTypes.R_FALSE, literal);
        b.classify(RElementTypes.R_FOR, keyword, cflow);
        b.classify(RElementTypes.R_FUNCTION, keyword);
        b.classify(RElementTypes.R_GE, binop, compare);
        b.classify(RElementTypes.R_GT, binop, compare);
        b.classify(RElementTypes.R_HELP, unop);
        b.classify(RElementTypes.R_IDENTIFIER, other);
        b.classify(RElementTypes.R_IF, keyword, cflow);
        b.classify(RElementTypes.R_IN, keyword);
        b.classify(RElementTypes.R_INF, literal);
        b.classify(RElementTypes.R_INFIX_OP, binop);
        b.classify(RElementTypes.R_INTEGER, literal);
        b.classify(RElementTypes.R_LBRACE, open);
        b.classify(RElementTypes.R_LBRACKET, open);
        b.classify(RElementTypes.R_LDBRACKET, open);
        b.classify(RElementTypes.R_LE, compare);
        b.classify(RElementTypes.R_LEFT_ASSIGN, assign);
        b.classify(RElementTypes.R_LEFT_COMPLEX_ASSIGN, assign);
        b.classify(RElementTypes.R_LIST_SUBSET, binop);
        b.classify(RElementTypes.R_LPAR, open);
        b.classify(RElementTypes.R_LT, compare);
        b.classify(RElementTypes.R_MINUS, unop, binop);
        b.classify(RElementTypes.R_MULT, binop);
        b.classify(RElementTypes.R_NA, literal);
        b.classify(RElementTypes.R_NAN, literal);
        b.classify(RElementTypes.R_NA_CHARACTER, literal);
        b.classify(RElementTypes.R_NA_COMPLEX, literal);
        b.classify(RElementTypes.R_NA_INTEGER, literal);
        b.classify(RElementTypes.R_NA_REAL, literal);
        b.classify(RElementTypes.R_NEXT, keyword, cflow);
        b.classify(RElementTypes.R_NL, separator);
        b.classify(RElementTypes.R_NOT, unop);
        b.classify(RElementTypes.R_NOTEQ, compare);
        b.classify(RElementTypes.R_NULL, literal);
        b.classify(RElementTypes.R_NUMERIC, literal);
        b.classify(RElementTypes.R_OR, binop);
        b.classify(RElementTypes.R_OROR, binop);
        b.classify(RElementTypes.R_PLUS, unop, binop);
        b.classify(RElementTypes.R_RBRACE, close);
        b.classify(RElementTypes.R_RBRACKET, close);
        b.classify(RElementTypes.R_RDBRACKET, close);
        b.classify(RElementTypes.R_REPEAT, keyword, cflow);
        b.classify(RElementTypes.R_RIGHT_ASSIGN, assign);
        b.classify(RElementTypes.R_RIGHT_COMPLEX_ASSIGN, assign);
        b.classify(RElementTypes.R_RPAR, close);
        b.classify(RElementTypes.R_SEMI, separator);
        b.classify(RElementTypes.R_STRING, literal);
        b.classify(RElementTypes.R_TICK, other);
        b.classify(RElementTypes.R_TILDE, binop);
        b.classify(RElementTypes.R_TRIPLECOLON, binop);
        b.classify(RElementTypes.R_TRIPLE_DOTS, literal);
        b.classify(RElementTypes.R_TRUE, literal);
        b.classify(RElementTypes.R_UNDERSCORE, literal);
        b.classify(RElementTypes.R_WHILE, keyword, cflow);

        STMT = b.get(stmt);
        EXPR = b.get(expr);
        UNOP = b.get(unop);
        BINOP = b.get(binop);
        BLOCK = b.get(indentParent);
        CFLOW = b.get(cflow);
        OPEN = b.get(open);
        CLOSE = b.get(close);
    }
}
