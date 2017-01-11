// This is a generated file. Not intended for manual editing.
package com.r4intellij.parsing;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RElementType;
import com.r4intellij.psi.stubs.RElementTypeFactory;
import com.r4intellij.psi.*;

public interface RElementTypes {

  IElementType THE_R_ARGUMENT_LIST = new RElementType("THE_R_ARGUMENT_LIST");
  IElementType THE_R_ASSIGNMENT_STATEMENT = RElementTypeFactory.getElementTypeByName("THE_R_ASSIGNMENT_STATEMENT");
  IElementType THE_R_AT_EXPRESSION = new RElementType("THE_R_AT_EXPRESSION");
  IElementType THE_R_BLOCK_EXPRESSION = new RElementType("THE_R_BLOCK_EXPRESSION");
  IElementType THE_R_BREAK_STATEMENT = new RElementType("THE_R_BREAK_STATEMENT");
  IElementType THE_R_CALL_EXPRESSION = new RElementType("THE_R_CALL_EXPRESSION");
  IElementType THE_R_EMPTY_EXPRESSION = new RElementType("THE_R_EMPTY_EXPRESSION");
  IElementType THE_R_EXPRESSION = new RElementType("THE_R_EXPRESSION");
  IElementType THE_R_FOR_STATEMENT = new RElementType("THE_R_FOR_STATEMENT");
  IElementType THE_R_FUNCTION_EXPRESSION = new RElementType("THE_R_FUNCTION_EXPRESSION");
  IElementType THE_R_HELP_EXPRESSION = new RElementType("THE_R_HELP_EXPRESSION");
  IElementType THE_R_IF_STATEMENT = new RElementType("THE_R_IF_STATEMENT");
  IElementType THE_R_LOGICAL_LITERAL_EXPRESSION = new RElementType("THE_R_LOGICAL_LITERAL_EXPRESSION");
  IElementType THE_R_MEMBER_EXPRESSION = new RElementType("THE_R_MEMBER_EXPRESSION");
  IElementType THE_R_NA_LITERAL_EXPRESSION = new RElementType("THE_R_NA_LITERAL_EXPRESSION");
  IElementType THE_R_NEXT_STATEMENT = new RElementType("THE_R_NEXT_STATEMENT");
  IElementType THE_R_NULL_LITERAL_EXPRESSION = new RElementType("THE_R_NULL_LITERAL_EXPRESSION");
  IElementType THE_R_NUMERIC_LITERAL_EXPRESSION = new RElementType("THE_R_NUMERIC_LITERAL_EXPRESSION");
  IElementType THE_R_OPERATOR = new RElementType("THE_R_OPERATOR");
  IElementType THE_R_OPERATOR_EXPRESSION = new RElementType("THE_R_OPERATOR_EXPRESSION");
  IElementType THE_R_PARAMETER = new RElementType("THE_R_PARAMETER");
  IElementType THE_R_PARAMETER_LIST = new RElementType("THE_R_PARAMETER_LIST");
  IElementType THE_R_PARENTHESIZED_EXPRESSION = new RElementType("THE_R_PARENTHESIZED_EXPRESSION");
  IElementType THE_R_REFERENCE_EXPRESSION = new RElementType("THE_R_REFERENCE_EXPRESSION");
  IElementType THE_R_REPEAT_STATEMENT = new RElementType("THE_R_REPEAT_STATEMENT");
  IElementType THE_R_SLICE_EXPRESSION = new RElementType("THE_R_SLICE_EXPRESSION");
  IElementType THE_R_STRING_LITERAL_EXPRESSION = new RElementType("THE_R_STRING_LITERAL_EXPRESSION");
  IElementType THE_R_SUBSCRIPTION_EXPRESSION = new RElementType("THE_R_SUBSCRIPTION_EXPRESSION");
  IElementType THE_R_TILDE_EXPRESSION = new RElementType("THE_R_TILDE_EXPRESSION");
  IElementType THE_R_UNARY_TILDE_EXPRESSION = new RElementType("THE_R_UNARY_TILDE_EXPRESSION");
  IElementType THE_R_WHILE_STATEMENT = new RElementType("THE_R_WHILE_STATEMENT");

  IElementType THE_R_AND = new RElementType("&");
  IElementType THE_R_ANDAND = new RElementType("&&");
  IElementType THE_R_AT = new RElementType("@");
  IElementType THE_R_BREAK = new RElementType("break");
  IElementType THE_R_COLON = new RElementType(":");
  IElementType THE_R_COMMA = new RElementType(",");
  IElementType THE_R_COMPLEX = new RElementType("complex");
  IElementType THE_R_DIV = new RElementType("/");
  IElementType THE_R_DOUBLECOLON = new RElementType("::");
  IElementType THE_R_ELSE = new RElementType("else");
  IElementType THE_R_EQ = new RElementType("=");
  IElementType THE_R_EQEQ = new RElementType("==");
  IElementType THE_R_EXP = new RElementType("^");
  IElementType THE_R_F = new RElementType("F");
  IElementType THE_R_FALSE = new RElementType("FALSE");
  IElementType THE_R_FOR = new RElementType("for");
  IElementType THE_R_FUNCTION = new RElementType("function");
  IElementType THE_R_GE = new RElementType(">=");
  IElementType THE_R_GT = new RElementType(">");
  IElementType THE_R_HELP = new RElementType("help");
  IElementType THE_R_IDENTIFIER = new RElementType("identifier");
  IElementType THE_R_IF = new RElementType("if");
  IElementType THE_R_IN = new RElementType("in");
  IElementType THE_R_INF = new RElementType("INF");
  IElementType THE_R_INFIX_OP = new RElementType("INFIX_OP");
  IElementType THE_R_INTEGER = new RElementType("integer");
  IElementType THE_R_LBRACE = new RElementType("{");
  IElementType THE_R_LBRACKET = new RElementType("[");
  IElementType THE_R_LDBRACKET = new RElementType("[[");
  IElementType THE_R_LE = new RElementType("<=");
  IElementType THE_R_LEFT_ASSIGN = new RElementType("<-");
  IElementType THE_R_LEFT_COMPLEX_ASSIGN = new RElementType("<<-");
  IElementType THE_R_LIST_SUBSET = new RElementType("$");
  IElementType THE_R_LPAR = new RElementType("(");
  IElementType THE_R_LT = new RElementType("<");
  IElementType THE_R_MINUS = new RElementType("-");
  IElementType THE_R_MULT = new RElementType("*");
  IElementType THE_R_NA = new RElementType("NA");
  IElementType THE_R_NAN = new RElementType("NAN");
  IElementType THE_R_NA_CHARACTER = new RElementType("NA_CHARACTER");
  IElementType THE_R_NA_COMPLEX = new RElementType("NA_COMPLEX");
  IElementType THE_R_NA_INTEGER = new RElementType("NA_INTEGER");
  IElementType THE_R_NA_REAL = new RElementType("NA_REAL");
  IElementType THE_R_NEXT = new RElementType("next");
  IElementType THE_R_NL = new RElementType("nl");
  IElementType THE_R_NOT = new RElementType("!");
  IElementType THE_R_NOTEQ = new RElementType("!=");
  IElementType THE_R_NULL = new RElementType("NULL");
  IElementType THE_R_NUMERIC = new RElementType("numeric");
  IElementType THE_R_OR = new RElementType("|");
  IElementType THE_R_OROR = new RElementType("||");
  IElementType THE_R_PLUS = new RElementType("+");
  IElementType THE_R_RBRACE = new RElementType("}");
  IElementType THE_R_RBRACKET = new RElementType("]");
  IElementType THE_R_RDBRACKET = new RElementType("]]");
  IElementType THE_R_REPEAT = new RElementType("repeat");
  IElementType THE_R_RIGHT_ASSIGN = new RElementType("->");
  IElementType THE_R_RIGHT_COMPLEX_ASSIGN = new RElementType("->>");
  IElementType THE_R_RPAR = new RElementType(")");
  IElementType THE_R_SEMI = new RElementType(";");
  IElementType THE_R_STRING = new RElementType("string");
  IElementType THE_R_T = new RElementType("T");
  IElementType THE_R_TILDE = new RElementType("~");
  IElementType THE_R_TRIPLECOLON = new RElementType(":::");
  IElementType THE_R_TRIPLE_DOTS = new RElementType("TRIPLE_DOTS");
  IElementType THE_R_TRUE = new RElementType("TRUE");
  IElementType THE_R_WHILE = new RElementType("while");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == THE_R_ARGUMENT_LIST) {
        return new RArgumentListImpl(node);
      }
      else if (type == THE_R_ASSIGNMENT_STATEMENT) {
        return new RAssignmentStatementImpl(node);
      }
      else if (type == THE_R_AT_EXPRESSION) {
        return new RAtExpressionImpl(node);
      }
      else if (type == THE_R_BLOCK_EXPRESSION) {
        return new RBlockExpressionImpl(node);
      }
      else if (type == THE_R_BREAK_STATEMENT) {
        return new RBreakStatementImpl(node);
      }
      else if (type == THE_R_CALL_EXPRESSION) {
        return new RCallExpressionImpl(node);
      }
      else if (type == THE_R_EMPTY_EXPRESSION) {
        return new REmptyExpressionImpl(node);
      }
      else if (type == THE_R_EXPRESSION) {
        return new RExpressionImpl(node);
      }
      else if (type == THE_R_FOR_STATEMENT) {
        return new RForStatementImpl(node);
      }
      else if (type == THE_R_FUNCTION_EXPRESSION) {
        return new RFunctionExpressionImpl(node);
      }
      else if (type == THE_R_HELP_EXPRESSION) {
        return new RHelpExpressionImpl(node);
      }
      else if (type == THE_R_IF_STATEMENT) {
        return new RIfStatementImpl(node);
      }
      else if (type == THE_R_LOGICAL_LITERAL_EXPRESSION) {
        return new RLogicalLiteralExpressionImpl(node);
      }
      else if (type == THE_R_MEMBER_EXPRESSION) {
        return new RMemberExpressionImpl(node);
      }
      else if (type == THE_R_NA_LITERAL_EXPRESSION) {
        return new RNaLiteralExpressionImpl(node);
      }
      else if (type == THE_R_NEXT_STATEMENT) {
        return new RNextStatementImpl(node);
      }
      else if (type == THE_R_NULL_LITERAL_EXPRESSION) {
        return new RNullLiteralExpressionImpl(node);
      }
      else if (type == THE_R_NUMERIC_LITERAL_EXPRESSION) {
        return new RNumericLiteralExpressionImpl(node);
      }
      else if (type == THE_R_OPERATOR) {
        return new ROperatorImpl(node);
      }
      else if (type == THE_R_OPERATOR_EXPRESSION) {
        return new ROperatorExpressionImpl(node);
      }
      else if (type == THE_R_PARAMETER) {
        return new RParameterImpl(node);
      }
      else if (type == THE_R_PARAMETER_LIST) {
        return new RParameterListImpl(node);
      }
      else if (type == THE_R_PARENTHESIZED_EXPRESSION) {
        return new RParenthesizedExpressionImpl(node);
      }
      else if (type == THE_R_REFERENCE_EXPRESSION) {
        return new RReferenceExpressionImpl(node);
      }
      else if (type == THE_R_REPEAT_STATEMENT) {
        return new RRepeatStatementImpl(node);
      }
      else if (type == THE_R_SLICE_EXPRESSION) {
        return new RSliceExpressionImpl(node);
      }
      else if (type == THE_R_STRING_LITERAL_EXPRESSION) {
        return new RStringLiteralExpressionImpl(node);
      }
      else if (type == THE_R_SUBSCRIPTION_EXPRESSION) {
        return new RSubscriptionExpressionImpl(node);
      }
      else if (type == THE_R_TILDE_EXPRESSION) {
        return new RTildeExpressionImpl(node);
      }
      else if (type == THE_R_UNARY_TILDE_EXPRESSION) {
        return new RUnaryTildeExpressionImpl(node);
      }
      else if (type == THE_R_WHILE_STATEMENT) {
        return new RWhileStatementImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
