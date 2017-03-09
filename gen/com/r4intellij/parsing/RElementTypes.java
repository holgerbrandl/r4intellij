// This is a generated file. Not intended for manual editing.
package com.r4intellij.parsing;

import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RElementType;
import com.r4intellij.psi.stubs.RElementTypeFactory;
import com.r4intellij.psi.*;

public interface RElementTypes {

  IElementType R_ARGUMENT_LIST = new RElementType("R_ARGUMENT_LIST");
  IElementType R_ASSIGNMENT_STATEMENT = RElementTypeFactory.getElementTypeByName("R_ASSIGNMENT_STATEMENT");
  IElementType R_AT_EXPRESSION = new RElementType("R_AT_EXPRESSION");
  IElementType R_BLOCK_EXPRESSION = new RElementType("R_BLOCK_EXPRESSION");
  IElementType R_BREAK_STATEMENT = new RElementType("R_BREAK_STATEMENT");
  IElementType R_CALL_EXPRESSION = new RElementType("R_CALL_EXPRESSION");
  IElementType R_EMPTY_EXPRESSION = new RElementType("R_EMPTY_EXPRESSION");
  IElementType R_EXPRESSION = new RElementType("R_EXPRESSION");
  IElementType R_FOR_STATEMENT = new RElementType("R_FOR_STATEMENT");
  IElementType R_FUNCTION_EXPRESSION = new RElementType("R_FUNCTION_EXPRESSION");
  IElementType R_HELP_EXPRESSION = new RElementType("R_HELP_EXPRESSION");
  IElementType R_IF_STATEMENT = new RElementType("R_IF_STATEMENT");
  IElementType R_LOGICAL_LITERAL_EXPRESSION = new RElementType("R_LOGICAL_LITERAL_EXPRESSION");
  IElementType R_MEMBER_EXPRESSION = new RElementType("R_MEMBER_EXPRESSION");
  IElementType R_NA_LITERAL_EXPRESSION = new RElementType("R_NA_LITERAL_EXPRESSION");
  IElementType R_NEXT_STATEMENT = new RElementType("R_NEXT_STATEMENT");
  IElementType R_NULL_LITERAL_EXPRESSION = new RElementType("R_NULL_LITERAL_EXPRESSION");
  IElementType R_NUMERIC_LITERAL_EXPRESSION = new RElementType("R_NUMERIC_LITERAL_EXPRESSION");
  IElementType R_OPERATOR = new RElementType("R_OPERATOR");
  IElementType R_OPERATOR_EXPRESSION = new RElementType("R_OPERATOR_EXPRESSION");
  IElementType R_PARAMETER = new RElementType("R_PARAMETER");
  IElementType R_PARAMETER_LIST = new RElementType("R_PARAMETER_LIST");
  IElementType R_PARENTHESIZED_EXPRESSION = new RElementType("R_PARENTHESIZED_EXPRESSION");
  IElementType R_REFERENCE_EXPRESSION = new RElementType("R_REFERENCE_EXPRESSION");
  IElementType R_REPEAT_STATEMENT = new RElementType("R_REPEAT_STATEMENT");
  IElementType R_STRING_LITERAL_EXPRESSION = new RElementType("R_STRING_LITERAL_EXPRESSION");
  IElementType R_SUBSCRIPTION_EXPRESSION = new RElementType("R_SUBSCRIPTION_EXPRESSION");
  IElementType R_TILDE_EXPRESSION = new RElementType("R_TILDE_EXPRESSION");
  IElementType R_UNARY_TILDE_EXPRESSION = new RElementType("R_UNARY_TILDE_EXPRESSION");
  IElementType R_WHILE_STATEMENT = new RElementType("R_WHILE_STATEMENT");

  IElementType R_AND = new RElementType("&");
  IElementType R_ANDAND = new RElementType("&&");
  IElementType R_AT = new RElementType("@");
  IElementType R_BREAK = new RElementType("break");
  IElementType R_COLON = new RElementType(":");
  IElementType R_COMMA = new RElementType(",");
  IElementType R_COMPLEX = new RElementType("complex");
  IElementType R_DIV = new RElementType("/");
  IElementType R_DOUBLECOLON = new RElementType("::");
  IElementType R_ELSE = new RElementType("else");
  IElementType R_EQ = new RElementType("=");
  IElementType R_EQEQ = new RElementType("==");
  IElementType R_EXP = new RElementType("^");
  IElementType R_F = new RElementType("F");
  IElementType R_FALSE = new RElementType("FALSE");
  IElementType R_FOR = new RElementType("for");
  IElementType R_FUNCTION = new RElementType("function");
  IElementType R_GE = new RElementType(">=");
  IElementType R_GT = new RElementType(">");
  IElementType R_HELP = new RElementType("help");
  IElementType R_IDENTIFIER = new RElementType("identifier");
  IElementType R_IF = new RElementType("if");
  IElementType R_IN = new RElementType("in");
  IElementType R_INF = new RElementType("INF");
  IElementType R_INFIX_OP = new RElementType("INFIX_OP");
  IElementType R_INTEGER = new RElementType("integer");
  IElementType R_LBRACE = new RElementType("{");
  IElementType R_LBRACKET = new RElementType("[");
  IElementType R_LDBRACKET = new RElementType("[[");
  IElementType R_LE = new RElementType("<=");
  IElementType R_LEFT_ASSIGN = new RElementType("<-");
  IElementType R_LEFT_COMPLEX_ASSIGN = new RElementType("<<-");
  IElementType R_LIST_SUBSET = new RElementType("$");
  IElementType R_LPAR = new RElementType("(");
  IElementType R_LT = new RElementType("<");
  IElementType R_MINUS = new RElementType("-");
  IElementType R_MULT = new RElementType("*");
  IElementType R_NA = new RElementType("NA");
  IElementType R_NAN = new RElementType("NAN");
  IElementType R_NA_CHARACTER = new RElementType("NA_CHARACTER");
  IElementType R_NA_COMPLEX = new RElementType("NA_COMPLEX");
  IElementType R_NA_INTEGER = new RElementType("NA_INTEGER");
  IElementType R_NA_REAL = new RElementType("NA_REAL");
  IElementType R_NEXT = new RElementType("next");
  IElementType R_NL = new RElementType("nl");
  IElementType R_NOT = new RElementType("!");
  IElementType R_NOTEQ = new RElementType("!=");
  IElementType R_NULL = new RElementType("NULL");
  IElementType R_NUMERIC = new RElementType("numeric");
  IElementType R_OR = new RElementType("|");
  IElementType R_OROR = new RElementType("||");
  IElementType R_PLUS = new RElementType("+");
  IElementType R_RBRACE = new RElementType("}");
  IElementType R_RBRACKET = new RElementType("]");
  IElementType R_RDBRACKET = new RElementType("]]");
  IElementType R_REPEAT = new RElementType("repeat");
  IElementType R_RIGHT_ASSIGN = new RElementType("->");
  IElementType R_RIGHT_COMPLEX_ASSIGN = new RElementType("->>");
  IElementType R_RPAR = new RElementType(")");
  IElementType R_SEMI = new RElementType(";");
  IElementType R_STRING = new RElementType("string");
  IElementType R_T = new RElementType("T");
  IElementType R_TICK = new RElementType("`");
  IElementType R_TILDE = new RElementType("~");
  IElementType R_TRIPLECOLON = new RElementType(":::");
  IElementType R_TRIPLE_DOTS = new RElementType("TRIPLE_DOTS");
  IElementType R_TRUE = new RElementType("TRUE");
  IElementType R_UNDERSCORE = new RElementType("_");
  IElementType R_WHILE = new RElementType("while");

  class Factory {
    public static PsiElement createElement(ASTNode node) {
      IElementType type = node.getElementType();
       if (type == R_ARGUMENT_LIST) {
        return new RArgumentListImpl(node);
      }
      else if (type == R_ASSIGNMENT_STATEMENT) {
        return new RAssignmentStatementImpl(node);
      }
      else if (type == R_AT_EXPRESSION) {
        return new RAtExpressionImpl(node);
      }
      else if (type == R_BLOCK_EXPRESSION) {
        return new RBlockExpressionImpl(node);
      }
      else if (type == R_BREAK_STATEMENT) {
        return new RBreakStatementImpl(node);
      }
      else if (type == R_CALL_EXPRESSION) {
        return new RCallExpressionImpl(node);
      }
      else if (type == R_EMPTY_EXPRESSION) {
        return new REmptyExpressionImpl(node);
      }
      else if (type == R_FOR_STATEMENT) {
        return new RForStatementImpl(node);
      }
      else if (type == R_FUNCTION_EXPRESSION) {
        return new RFunctionExpressionImpl(node);
      }
      else if (type == R_HELP_EXPRESSION) {
        return new RHelpExpressionImpl(node);
      }
      else if (type == R_IF_STATEMENT) {
        return new RIfStatementImpl(node);
      }
      else if (type == R_LOGICAL_LITERAL_EXPRESSION) {
        return new RLogicalLiteralExpressionImpl(node);
      }
      else if (type == R_MEMBER_EXPRESSION) {
        return new RMemberExpressionImpl(node);
      }
      else if (type == R_NA_LITERAL_EXPRESSION) {
        return new RNaLiteralExpressionImpl(node);
      }
      else if (type == R_NEXT_STATEMENT) {
        return new RNextStatementImpl(node);
      }
      else if (type == R_NULL_LITERAL_EXPRESSION) {
        return new RNullLiteralExpressionImpl(node);
      }
      else if (type == R_NUMERIC_LITERAL_EXPRESSION) {
        return new RNumericLiteralExpressionImpl(node);
      }
      else if (type == R_OPERATOR) {
        return new ROperatorImpl(node);
      }
      else if (type == R_OPERATOR_EXPRESSION) {
        return new ROperatorExpressionImpl(node);
      }
      else if (type == R_PARAMETER) {
        return new RParameterImpl(node);
      }
      else if (type == R_PARAMETER_LIST) {
        return new RParameterListImpl(node);
      }
      else if (type == R_PARENTHESIZED_EXPRESSION) {
        return new RParenthesizedExpressionImpl(node);
      }
      else if (type == R_REFERENCE_EXPRESSION) {
        return new RReferenceExpressionImpl(node);
      }
      else if (type == R_REPEAT_STATEMENT) {
        return new RRepeatStatementImpl(node);
      }
      else if (type == R_STRING_LITERAL_EXPRESSION) {
        return new RStringLiteralExpressionImpl(node);
      }
      else if (type == R_SUBSCRIPTION_EXPRESSION) {
        return new RSubscriptionExpressionImpl(node);
      }
      else if (type == R_TILDE_EXPRESSION) {
        return new RTildeExpressionImpl(node);
      }
      else if (type == R_UNARY_TILDE_EXPRESSION) {
        return new RUnaryTildeExpressionImpl(node);
      }
      else if (type == R_WHILE_STATEMENT) {
        return new RWhileStatementImpl(node);
      }
      throw new AssertionError("Unknown element type: " + type);
    }
  }
}
