// This is a generated file. Not intended for manual editing.
package com.r4intellij.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static com.r4intellij.parsing.RElementTypes.*;
import static com.r4intellij.parsing.RParserUtil.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;
import com.intellij.lang.LightPsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class RParser implements PsiParser, LightPsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, EXTENDS_SETS_);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    if (t == R_ARGUMENT_LIST) {
      r = argument_list(b, 0);
    }
    else if (t == R_ASSIGNMENT_STATEMENT) {
      r = assignment_statement(b, 0);
    }
    else if (t == R_EMPTY_EXPRESSION) {
      r = empty_expression(b, 0);
    }
    else if (t == R_EXPRESSION) {
      r = expression(b, 0, -1);
    }
    else if (t == R_OPERATOR) {
      r = operator(b, 0);
    }
    else if (t == R_OPERATOR_EXPRESSION) {
      r = operator_expression(b, 0);
    }
    else if (t == R_PARAMETER) {
      r = parameter(b, 0);
    }
    else if (t == R_PARAMETER_LIST) {
      r = parameter_list(b, 0);
    }
    else {
      r = parse_root_(t, b, 0);
    }
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return root(b, l + 1);
  }

  public static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    create_token_set_(R_ASSIGNMENT_STATEMENT, R_AT_EXPRESSION, R_BLOCK_EXPRESSION, R_BREAK_STATEMENT,
      R_CALL_EXPRESSION, R_EMPTY_EXPRESSION, R_EXPRESSION, R_FOR_STATEMENT,
      R_FUNCTION_EXPRESSION, R_HELP_EXPRESSION, R_IF_STATEMENT, R_LOGICAL_LITERAL_EXPRESSION,
      R_MEMBER_EXPRESSION, R_NA_LITERAL_EXPRESSION, R_NEXT_STATEMENT, R_NULL_LITERAL_EXPRESSION,
      R_NUMERIC_LITERAL_EXPRESSION, R_OPERATOR_EXPRESSION, R_PARENTHESIZED_EXPRESSION, R_REFERENCE_EXPRESSION,
      R_REPEAT_STATEMENT, R_STRING_LITERAL_EXPRESSION, R_SUBSCRIPTION_EXPRESSION, R_TILDE_EXPRESSION,
      R_UNARY_TILDE_EXPRESSION, R_WHILE_STATEMENT),
  };

  /* ********************************************************** */
  // '&' | '&&'
  public static boolean and_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "and_operator")) return false;
    if (!nextTokenIs(b, "<and operator>", R_ANDAND, R_AND)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_OPERATOR, "<and operator>");
    r = consumeToken(b, R_AND);
    if (!r) r = consumeToken(b, R_ANDAND);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '...' | expression | external_empty_expression
  static boolean arg(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "arg")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "...");
    if (!r) r = expression(b, l + 1, -1);
    if (!r) r = parseEmptyExpression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' nl* ')' | '(' nl* arg nl* (',' nl* arg nl*)* ')'
  public static boolean argument_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list")) return false;
    if (!nextTokenIs(b, R_LPAR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = argument_list_0(b, l + 1);
    if (!r) r = argument_list_1(b, l + 1);
    exit_section_(b, m, R_ARGUMENT_LIST, r);
    return r;
  }

  // '(' nl* ')'
  private static boolean argument_list_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_LPAR);
    r = r && argument_list_0_1(b, l + 1);
    r = r && consumeToken(b, R_RPAR);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean argument_list_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "argument_list_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '(' nl* arg nl* (',' nl* arg nl*)* ')'
  private static boolean argument_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_LPAR);
    r = r && argument_list_1_1(b, l + 1);
    r = r && arg(b, l + 1);
    r = r && argument_list_1_3(b, l + 1);
    r = r && argument_list_1_4(b, l + 1);
    r = r && consumeToken(b, R_RPAR);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean argument_list_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "argument_list_1_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean argument_list_1_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "argument_list_1_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // (',' nl* arg nl*)*
  private static boolean argument_list_1_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1_4")) return false;
    int c = current_position_(b);
    while (true) {
      if (!argument_list_1_4_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "argument_list_1_4", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' nl* arg nl*
  private static boolean argument_list_1_4_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1_4_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_COMMA);
    r = r && argument_list_1_4_0_1(b, l + 1);
    r = r && arg(b, l + 1);
    r = r && argument_list_1_4_0_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean argument_list_1_4_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1_4_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "argument_list_1_4_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean argument_list_1_4_0_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "argument_list_1_4_0_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "argument_list_1_4_0_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  public static boolean assignment_statement(PsiBuilder b, int l) {
    Marker m = enter_section_(b);
    exit_section_(b, m, R_ASSIGNMENT_STATEMENT, true);
    return true;
  }

  /* ********************************************************** */
  // nl* '(' nl* (expression nl*)? ')'
  static boolean break_next_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_next_expression")) return false;
    if (!nextTokenIs(b, "", R_LPAR, R_NL)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = break_next_expression_0(b, l + 1);
    r = r && consumeToken(b, R_LPAR);
    r = r && break_next_expression_2(b, l + 1);
    r = r && break_next_expression_3(b, l + 1);
    r = r && consumeToken(b, R_RPAR);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean break_next_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_next_expression_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "break_next_expression_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean break_next_expression_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_next_expression_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "break_next_expression_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // (expression nl*)?
  private static boolean break_next_expression_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_next_expression_3")) return false;
    break_next_expression_3_0(b, l + 1);
    return true;
  }

  // expression nl*
  private static boolean break_next_expression_3_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_next_expression_3_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = expression(b, l + 1, -1);
    r = r && break_next_expression_3_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean break_next_expression_3_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_next_expression_3_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "break_next_expression_3_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // ':'
  public static boolean colon_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "colon_operator")) return false;
    if (!nextTokenIs(b, R_COLON)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_COLON);
    exit_section_(b, m, R_OPERATOR, r);
    return r;
  }

  /* ********************************************************** */
  // '>' | '>=' | '<' | '<=' | '==' | '!='
  public static boolean compare_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compare_operator")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_OPERATOR, "<compare operator>");
    r = consumeToken(b, R_GT);
    if (!r) r = consumeToken(b, R_GE);
    if (!r) r = consumeToken(b, R_LT);
    if (!r) r = consumeToken(b, R_LE);
    if (!r) r = consumeToken(b, R_EQEQ);
    if (!r) r = consumeToken(b, R_NOTEQ);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // expression
  public static boolean empty_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "empty_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, R_EMPTY_EXPRESSION, "<empty expression>");
    r = expression(b, l + 1, -1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '='
  public static boolean eq_assign_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "eq_assign_operator")) return false;
    if (!nextTokenIs(b, R_EQ)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_EQ);
    exit_section_(b, m, R_OPERATOR, r);
    return r;
  }

  /* ********************************************************** */
  // '^'
  public static boolean exp_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_operator")) return false;
    if (!nextTokenIs(b, R_EXP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_EXP);
    exit_section_(b, m, R_OPERATOR, r);
    return r;
  }

  /* ********************************************************** */
  // expression? (semicolon+ expression?)*
  static boolean expression_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expression_list")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = expression_list_0(b, l + 1);
    r = r && expression_list_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // expression?
  private static boolean expression_list_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expression_list_0")) return false;
    expression(b, l + 1, -1);
    return true;
  }

  // (semicolon+ expression?)*
  private static boolean expression_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expression_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!expression_list_1_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "expression_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // semicolon+ expression?
  private static boolean expression_list_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expression_list_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = expression_list_1_0_0(b, l + 1);
    r = r && expression_list_1_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // semicolon+
  private static boolean expression_list_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expression_list_1_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = semicolon(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!semicolon(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "expression_list_1_0_0", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, m, null, r);
    return r;
  }

  // expression?
  private static boolean expression_list_1_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expression_list_1_0_1")) return false;
    expression(b, l + 1, -1);
    return true;
  }

  /* ********************************************************** */
  // INFIX_OP
  public static boolean infix_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infix_operator")) return false;
    if (!nextTokenIs(b, R_INFIX_OP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_INFIX_OP);
    exit_section_(b, m, R_OPERATOR, r);
    return r;
  }

  /* ********************************************************** */
  // NA_INTEGER | NA_REAL | NA_COMPLEX | NA_CHARACTER |
  //   TRIPLE_DOTS | if | else | repeat | while |
  //   function | for | in | next | break
  static boolean keyword(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "keyword")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_NA_INTEGER);
    if (!r) r = consumeToken(b, R_NA_REAL);
    if (!r) r = consumeToken(b, R_NA_COMPLEX);
    if (!r) r = consumeToken(b, R_NA_CHARACTER);
    if (!r) r = consumeToken(b, R_TRIPLE_DOTS);
    if (!r) r = consumeToken(b, R_IF);
    if (!r) r = consumeToken(b, R_ELSE);
    if (!r) r = consumeToken(b, R_REPEAT);
    if (!r) r = consumeToken(b, R_WHILE);
    if (!r) r = consumeToken(b, R_FUNCTION);
    if (!r) r = consumeToken(b, R_FOR);
    if (!r) r = consumeToken(b, R_IN);
    if (!r) r = consumeToken(b, R_NEXT);
    if (!r) r = consumeToken(b, R_BREAK);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '<-' | '<<-'
  public static boolean left_assign_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "left_assign_operator")) return false;
    if (!nextTokenIs(b, "<left assign operator>", R_LEFT_ASSIGN, R_LEFT_COMPLEX_ASSIGN)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_OPERATOR, "<left assign operator>");
    r = consumeToken(b, R_LEFT_ASSIGN);
    if (!r) r = consumeToken(b, R_LEFT_COMPLEX_ASSIGN);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // identifier | string | '...'
  static boolean member_tag(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "member_tag")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_IDENTIFIER);
    if (!r) r = consumeToken(b, R_STRING);
    if (!r) r = consumeToken(b, "...");
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '*' | '/'
  public static boolean muldiv_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "muldiv_operator")) return false;
    if (!nextTokenIs(b, "<muldiv operator>", R_MULT, R_DIV)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_OPERATOR, "<muldiv operator>");
    r = consumeToken(b, R_MULT);
    if (!r) r = consumeToken(b, R_DIV);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '!'
  public static boolean not_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "not_operator")) return false;
    if (!nextTokenIs(b, R_NOT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_NOT);
    exit_section_(b, m, R_OPERATOR, r);
    return r;
  }

  /* ********************************************************** */
  public static boolean operator(PsiBuilder b, int l) {
    Marker m = enter_section_(b);
    exit_section_(b, m, R_OPERATOR, true);
    return true;
  }

  /* ********************************************************** */
  // expression
  public static boolean operator_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "operator_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, R_OPERATOR_EXPRESSION, "<operator expression>");
    r = expression(b, l + 1, -1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '|' | '||'
  public static boolean or_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "or_operator")) return false;
    if (!nextTokenIs(b, "<or operator>", R_OR, R_OROR)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_OPERATOR, "<or operator>");
    r = consumeToken(b, R_OR);
    if (!r) r = consumeToken(b, R_OROR);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // (identifier '=' expression) | identifier | '...'
  public static boolean parameter(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_PARAMETER, "<parameter>");
    r = parameter_0(b, l + 1);
    if (!r) r = consumeToken(b, R_IDENTIFIER);
    if (!r) r = consumeToken(b, "...");
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // identifier '=' expression
  private static boolean parameter_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, R_IDENTIFIER, R_EQ);
    r = r && expression(b, l + 1, -1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // ('(' ')') | ('(' nl* parameter nl* (',' nl* parameter nl*)* ')')
  public static boolean parameter_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list")) return false;
    if (!nextTokenIs(b, R_LPAR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = parameter_list_0(b, l + 1);
    if (!r) r = parameter_list_1(b, l + 1);
    exit_section_(b, m, R_PARAMETER_LIST, r);
    return r;
  }

  // '(' ')'
  private static boolean parameter_list_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokens(b, 0, R_LPAR, R_RPAR);
    exit_section_(b, m, null, r);
    return r;
  }

  // '(' nl* parameter nl* (',' nl* parameter nl*)* ')'
  private static boolean parameter_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_LPAR);
    r = r && parameter_list_1_1(b, l + 1);
    r = r && parameter(b, l + 1);
    r = r && parameter_list_1_3(b, l + 1);
    r = r && parameter_list_1_4(b, l + 1);
    r = r && consumeToken(b, R_RPAR);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean parameter_list_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_1_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "parameter_list_1_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean parameter_list_1_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_1_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "parameter_list_1_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // (',' nl* parameter nl*)*
  private static boolean parameter_list_1_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_1_4")) return false;
    int c = current_position_(b);
    while (true) {
      if (!parameter_list_1_4_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "parameter_list_1_4", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' nl* parameter nl*
  private static boolean parameter_list_1_4_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_1_4_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_COMMA);
    r = r && parameter_list_1_4_0_1(b, l + 1);
    r = r && parameter(b, l + 1);
    r = r && parameter_list_1_4_0_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean parameter_list_1_4_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_1_4_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "parameter_list_1_4_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean parameter_list_1_4_0_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parameter_list_1_4_0_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "parameter_list_1_4_0_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // '+' | '-'
  public static boolean plusminus_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "plusminus_operator")) return false;
    if (!nextTokenIs(b, "<plusminus operator>", R_PLUS, R_MINUS)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_OPERATOR, "<plusminus operator>");
    r = consumeToken(b, R_PLUS);
    if (!r) r = consumeToken(b, R_MINUS);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '->' | '->>'
  public static boolean right_assign_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "right_assign_operator")) return false;
    if (!nextTokenIs(b, "<right assign operator>", R_RIGHT_ASSIGN, R_RIGHT_COMPLEX_ASSIGN)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_OPERATOR, "<right assign operator>");
    r = consumeToken(b, R_RIGHT_ASSIGN);
    if (!r) r = consumeToken(b, R_RIGHT_COMPLEX_ASSIGN);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // expression_list
  static boolean root(PsiBuilder b, int l) {
    return expression_list(b, l + 1);
  }

  /* ********************************************************** */
  // ';' | nl
  static boolean semicolon(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "semicolon")) return false;
    if (!nextTokenIs(b, "", R_SEMI, R_NL)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_SEMI);
    if (!r) r = consumeToken(b, R_NL);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // INF | NAN
  static boolean special_constant(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "special_constant")) return false;
    if (!nextTokenIs(b, "", R_INF, R_NAN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_INF);
    if (!r) r = consumeToken(b, R_NAN);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '...' | expression | external_empty_expression
  static boolean subscription_expr_elem(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expr_elem")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, "...");
    if (!r) r = expression(b, l + 1, -1);
    if (!r) r = parseEmptyExpression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // subscription_expr_elem nl* (',' nl* subscription_expr_elem nl*)*
  static boolean subscription_expr_list(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expr_list")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = subscription_expr_elem(b, l + 1);
    r = r && subscription_expr_list_1(b, l + 1);
    r = r && subscription_expr_list_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean subscription_expr_list_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expr_list_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expr_list_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // (',' nl* subscription_expr_elem nl*)*
  private static boolean subscription_expr_list_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expr_list_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!subscription_expr_list_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expr_list_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ',' nl* subscription_expr_elem nl*
  private static boolean subscription_expr_list_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expr_list_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_COMMA);
    r = r && subscription_expr_list_2_0_1(b, l + 1);
    r = r && subscription_expr_elem(b, l + 1);
    r = r && subscription_expr_list_2_0_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean subscription_expr_list_2_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expr_list_2_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expr_list_2_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean subscription_expr_list_2_0_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expr_list_2_0_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expr_list_2_0_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // '~'
  public static boolean tilde_operator(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tilde_operator")) return false;
    if (!nextTokenIs(b, R_TILDE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, R_TILDE);
    exit_section_(b, m, R_OPERATOR, r);
    return r;
  }

  /* ********************************************************** */
  // Expression root: expression
  // Operator priority table:
  // 0: ATOM(if_statement)
  // 1: ATOM(while_statement)
  // 2: ATOM(for_statement)
  // 3: PREFIX(repeat_statement)
  // 4: ATOM(break_statement)
  // 5: ATOM(next_statement)
  // 6: ATOM(block_expression)
  // 7: ATOM(help_expression)
  // 8: PREFIX(parenthesized_expression)
  // 9: PREFIX(function_expression)
  // 10: BINARY(left_assign_expression)
  // 11: POSTFIX(eq_assign_expression)
  // 12: BINARY(right_assign_expression)
  // 13: PREFIX(unary_tilde_expression) BINARY(tilde_expression)
  // 14: BINARY(or_expression)
  // 15: BINARY(and_expression)
  // 16: PREFIX(unary_not_expression)
  // 17: BINARY(compare_expression)
  // 18: BINARY(plusminus_expression)
  // 19: BINARY(muldiv_expression)
  // 20: BINARY(infix_expression)
  // 21: BINARY(colon_expression)
  // 22: PREFIX(unary_plusminus_expression)
  // 23: BINARY(exp_expression)
  // 24: POSTFIX(subscription_expression)
  // 25: POSTFIX(call_expression)
  // 26: POSTFIX(member_expression)
  // 27: POSTFIX(at_expression)
  // 28: POSTFIX(namespace_access_expression)
  // 29: ATOM(reference_expression) ATOM(numeric_literal_expression) ATOM(string_literal_expression) ATOM(logical_literal_expression)
  //    ATOM(null_literal_expression) ATOM(na_literal_expression)
  public static boolean expression(PsiBuilder b, int l, int g) {
    if (!recursion_guard_(b, l, "expression")) return false;
    addVariant(b, "<expression>");
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<expression>");
    r = if_statement(b, l + 1);
    if (!r) r = while_statement(b, l + 1);
    if (!r) r = for_statement(b, l + 1);
    if (!r) r = repeat_statement(b, l + 1);
    if (!r) r = break_statement(b, l + 1);
    if (!r) r = next_statement(b, l + 1);
    if (!r) r = block_expression(b, l + 1);
    if (!r) r = help_expression(b, l + 1);
    if (!r) r = parenthesized_expression(b, l + 1);
    if (!r) r = function_expression(b, l + 1);
    if (!r) r = unary_tilde_expression(b, l + 1);
    if (!r) r = unary_not_expression(b, l + 1);
    if (!r) r = unary_plusminus_expression(b, l + 1);
    if (!r) r = reference_expression(b, l + 1);
    if (!r) r = numeric_literal_expression(b, l + 1);
    if (!r) r = string_literal_expression(b, l + 1);
    if (!r) r = logical_literal_expression(b, l + 1);
    if (!r) r = null_literal_expression(b, l + 1);
    if (!r) r = na_literal_expression(b, l + 1);
    p = r;
    r = r && expression_0(b, l + 1, g);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  public static boolean expression_0(PsiBuilder b, int l, int g) {
    if (!recursion_guard_(b, l, "expression_0")) return false;
    boolean r = true;
    while (true) {
      Marker m = enter_section_(b, l, _LEFT_, null);
      if (g < 10 && left_assign_expression_0(b, l + 1)) {
        r = expression(b, l, 9);
        exit_section_(b, l, m, R_ASSIGNMENT_STATEMENT, r, true, null);
      }
      else if (g < 11 && eq_assign_expression_0(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, R_ASSIGNMENT_STATEMENT, r, true, null);
      }
      else if (g < 12 && right_assign_expression_0(b, l + 1)) {
        r = expression(b, l, 12);
        exit_section_(b, l, m, R_ASSIGNMENT_STATEMENT, r, true, null);
      }
      else if (g < 13 && tilde_expression_0(b, l + 1)) {
        r = expression(b, l, 13);
        exit_section_(b, l, m, R_TILDE_EXPRESSION, r, true, null);
      }
      else if (g < 14 && or_expression_0(b, l + 1)) {
        r = expression(b, l, 14);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 15 && and_expression_0(b, l + 1)) {
        r = expression(b, l, 15);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 17 && compare_expression_0(b, l + 1)) {
        r = expression(b, l, 17);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 18 && plusminus_expression_0(b, l + 1)) {
        r = expression(b, l, 18);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 19 && muldiv_expression_0(b, l + 1)) {
        r = expression(b, l, 19);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 20 && infix_expression_0(b, l + 1)) {
        r = expression(b, l, 20);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 21 && colon_expression_0(b, l + 1)) {
        r = expression(b, l, 21);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 23 && exp_expression_0(b, l + 1)) {
        r = expression(b, l, 23);
        exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, true, null);
      }
      else if (g < 24 && subscription_expression_0(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, R_SUBSCRIPTION_EXPRESSION, r, true, null);
      }
      else if (g < 25 && argument_list(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, R_CALL_EXPRESSION, r, true, null);
      }
      else if (g < 26 && member_expression_0(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, R_MEMBER_EXPRESSION, r, true, null);
      }
      else if (g < 27 && at_expression_0(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, R_AT_EXPRESSION, r, true, null);
      }
      else if (g < 28 && namespace_access_expression_0(b, l + 1)) {
        r = true;
        exit_section_(b, l, m, R_REFERENCE_EXPRESSION, r, true, null);
      }
      else {
        exit_section_(b, l, m, null, false, false, null);
        break;
      }
    }
    return r;
  }

  // if nl* '(' nl* expression nl* ')' nl* expression (nl* else nl* expression)?
  public static boolean if_statement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement")) return false;
    if (!nextTokenIsSmart(b, R_IF)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_IF);
    r = r && if_statement_1(b, l + 1);
    r = r && consumeToken(b, R_LPAR);
    r = r && if_statement_3(b, l + 1);
    r = r && expression(b, l + 1, -1);
    r = r && if_statement_5(b, l + 1);
    r = r && consumeToken(b, R_RPAR);
    r = r && if_statement_7(b, l + 1);
    r = r && expression(b, l + 1, -1);
    r = r && if_statement_9(b, l + 1);
    exit_section_(b, m, R_IF_STATEMENT, r);
    return r;
  }

  // nl*
  private static boolean if_statement_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "if_statement_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean if_statement_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "if_statement_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean if_statement_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_5")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "if_statement_5", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean if_statement_7(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_7")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "if_statement_7", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // (nl* else nl* expression)?
  private static boolean if_statement_9(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_9")) return false;
    if_statement_9_0(b, l + 1);
    return true;
  }

  // nl* else nl* expression
  private static boolean if_statement_9_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_9_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = if_statement_9_0_0(b, l + 1);
    r = r && consumeToken(b, R_ELSE);
    r = r && if_statement_9_0_2(b, l + 1);
    r = r && expression(b, l + 1, -1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean if_statement_9_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_9_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "if_statement_9_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean if_statement_9_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "if_statement_9_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "if_statement_9_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // while nl* '(' nl* expression nl* ')' nl* expression
  public static boolean while_statement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "while_statement")) return false;
    if (!nextTokenIsSmart(b, R_WHILE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_WHILE);
    r = r && while_statement_1(b, l + 1);
    r = r && consumeToken(b, R_LPAR);
    r = r && while_statement_3(b, l + 1);
    r = r && expression(b, l + 1, -1);
    r = r && while_statement_5(b, l + 1);
    r = r && consumeToken(b, R_RPAR);
    r = r && while_statement_7(b, l + 1);
    r = r && expression(b, l + 1, -1);
    exit_section_(b, m, R_WHILE_STATEMENT, r);
    return r;
  }

  // nl*
  private static boolean while_statement_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "while_statement_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "while_statement_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean while_statement_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "while_statement_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "while_statement_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean while_statement_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "while_statement_5")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "while_statement_5", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean while_statement_7(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "while_statement_7")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "while_statement_7", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // for nl* '(' nl* expression 'in' nl* expression ')' nl* expression
  public static boolean for_statement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "for_statement")) return false;
    if (!nextTokenIsSmart(b, R_FOR)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_FOR);
    r = r && for_statement_1(b, l + 1);
    r = r && consumeToken(b, R_LPAR);
    r = r && for_statement_3(b, l + 1);
    r = r && expression(b, l + 1, -1);
    r = r && consumeToken(b, "in");
    r = r && for_statement_6(b, l + 1);
    r = r && expression(b, l + 1, -1);
    r = r && consumeToken(b, R_RPAR);
    r = r && for_statement_9(b, l + 1);
    r = r && expression(b, l + 1, -1);
    exit_section_(b, m, R_FOR_STATEMENT, r);
    return r;
  }

  // nl*
  private static boolean for_statement_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "for_statement_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "for_statement_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean for_statement_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "for_statement_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "for_statement_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean for_statement_6(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "for_statement_6")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "for_statement_6", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean for_statement_9(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "for_statement_9")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "for_statement_9", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  public static boolean repeat_statement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "repeat_statement")) return false;
    if (!nextTokenIsSmart(b, R_REPEAT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = repeat_statement_0(b, l + 1);
    p = r;
    r = p && expression(b, l, 3);
    exit_section_(b, l, m, R_REPEAT_STATEMENT, r, p, null);
    return r || p;
  }

  // repeat nl*
  private static boolean repeat_statement_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "repeat_statement_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_REPEAT);
    r = r && repeat_statement_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean repeat_statement_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "repeat_statement_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "repeat_statement_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // break break_next_expression?
  public static boolean break_statement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_statement")) return false;
    if (!nextTokenIsSmart(b, R_BREAK)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_BREAK);
    r = r && break_statement_1(b, l + 1);
    exit_section_(b, m, R_BREAK_STATEMENT, r);
    return r;
  }

  // break_next_expression?
  private static boolean break_statement_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "break_statement_1")) return false;
    break_next_expression(b, l + 1);
    return true;
  }

  // next break_next_expression?
  public static boolean next_statement(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "next_statement")) return false;
    if (!nextTokenIsSmart(b, R_NEXT)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_NEXT);
    r = r && next_statement_1(b, l + 1);
    exit_section_(b, m, R_NEXT_STATEMENT, r);
    return r;
  }

  // break_next_expression?
  private static boolean next_statement_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "next_statement_1")) return false;
    break_next_expression(b, l + 1);
    return true;
  }

  // '{' nl* expression_list? nl* '}'
  public static boolean block_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "block_expression")) return false;
    if (!nextTokenIsSmart(b, R_LBRACE)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_LBRACE);
    r = r && block_expression_1(b, l + 1);
    r = r && block_expression_2(b, l + 1);
    r = r && block_expression_3(b, l + 1);
    r = r && consumeToken(b, R_RBRACE);
    exit_section_(b, m, R_BLOCK_EXPRESSION, r);
    return r;
  }

  // nl*
  private static boolean block_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "block_expression_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "block_expression_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // expression_list?
  private static boolean block_expression_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "block_expression_2")) return false;
    expression_list(b, l + 1);
    return true;
  }

  // nl*
  private static boolean block_expression_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "block_expression_3")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "block_expression_3", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // help (nl* help)? nl* (keyword | expression)
  public static boolean help_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "help_expression")) return false;
    if (!nextTokenIsSmart(b, R_HELP)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_HELP);
    r = r && help_expression_1(b, l + 1);
    r = r && help_expression_2(b, l + 1);
    r = r && help_expression_3(b, l + 1);
    exit_section_(b, m, R_HELP_EXPRESSION, r);
    return r;
  }

  // (nl* help)?
  private static boolean help_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "help_expression_1")) return false;
    help_expression_1_0(b, l + 1);
    return true;
  }

  // nl* help
  private static boolean help_expression_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "help_expression_1_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = help_expression_1_0_0(b, l + 1);
    r = r && consumeToken(b, R_HELP);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean help_expression_1_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "help_expression_1_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "help_expression_1_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean help_expression_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "help_expression_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "help_expression_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // keyword | expression
  private static boolean help_expression_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "help_expression_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = keyword(b, l + 1);
    if (!r) r = expression(b, l + 1, -1);
    exit_section_(b, m, null, r);
    return r;
  }

  public static boolean parenthesized_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesized_expression")) return false;
    if (!nextTokenIsSmart(b, R_LPAR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = parenthesized_expression_0(b, l + 1);
    p = r;
    r = p && expression(b, l, 8);
    r = p && report_error_(b, parenthesized_expression_1(b, l + 1)) && r;
    exit_section_(b, l, m, R_PARENTHESIZED_EXPRESSION, r, p, null);
    return r || p;
  }

  // '(' nl*
  private static boolean parenthesized_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesized_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_LPAR);
    r = r && parenthesized_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean parenthesized_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesized_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "parenthesized_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl* ')'
  private static boolean parenthesized_expression_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesized_expression_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = parenthesized_expression_1_0(b, l + 1);
    r = r && consumeToken(b, R_RPAR);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean parenthesized_expression_1_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "parenthesized_expression_1_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeToken(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "parenthesized_expression_1_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  public static boolean function_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_expression")) return false;
    if (!nextTokenIsSmart(b, R_FUNCTION)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = function_expression_0(b, l + 1);
    p = r;
    r = p && expression(b, l, 9);
    exit_section_(b, l, m, R_FUNCTION_EXPRESSION, r, p, null);
    return r || p;
  }

  // function parameter_list nl*
  private static boolean function_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_FUNCTION);
    r = r && parameter_list(b, l + 1);
    r = r && function_expression_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean function_expression_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "function_expression_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "function_expression_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // left_assign_operator nl*
  private static boolean left_assign_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "left_assign_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = left_assign_operator(b, l + 1);
    r = r && left_assign_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean left_assign_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "left_assign_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "left_assign_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // eq_assign_operator nl* (expression | external_empty_expression)
  private static boolean eq_assign_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "eq_assign_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = eq_assign_operator(b, l + 1);
    r = r && eq_assign_expression_0_1(b, l + 1);
    r = r && eq_assign_expression_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean eq_assign_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "eq_assign_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "eq_assign_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // expression | external_empty_expression
  private static boolean eq_assign_expression_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "eq_assign_expression_0_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = expression(b, l + 1, -1);
    if (!r) r = parseEmptyExpression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // right_assign_operator nl*
  private static boolean right_assign_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "right_assign_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = right_assign_operator(b, l + 1);
    r = r && right_assign_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean right_assign_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "right_assign_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "right_assign_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  public static boolean unary_tilde_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_tilde_expression")) return false;
    if (!nextTokenIsSmart(b, R_TILDE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = unary_tilde_expression_0(b, l + 1);
    p = r;
    r = p && expression(b, l, 13);
    exit_section_(b, l, m, R_UNARY_TILDE_EXPRESSION, r, p, null);
    return r || p;
  }

  // tilde_operator nl*
  private static boolean unary_tilde_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_tilde_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tilde_operator(b, l + 1);
    r = r && unary_tilde_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean unary_tilde_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_tilde_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "unary_tilde_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // tilde_operator nl*
  private static boolean tilde_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tilde_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = tilde_operator(b, l + 1);
    r = r && tilde_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean tilde_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "tilde_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "tilde_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl* or_operator nl*
  private static boolean or_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "or_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = or_expression_0_0(b, l + 1);
    r = r && or_operator(b, l + 1);
    r = r && or_expression_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean or_expression_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "or_expression_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "or_expression_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean or_expression_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "or_expression_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "or_expression_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl* and_operator nl*
  private static boolean and_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "and_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = and_expression_0_0(b, l + 1);
    r = r && and_operator(b, l + 1);
    r = r && and_expression_0_2(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean and_expression_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "and_expression_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "and_expression_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // nl*
  private static boolean and_expression_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "and_expression_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "and_expression_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  public static boolean unary_not_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_not_expression")) return false;
    if (!nextTokenIsSmart(b, R_NOT)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = unary_not_expression_0(b, l + 1);
    p = r;
    r = p && expression(b, l, 16);
    exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, p, null);
    return r || p;
  }

  // not_operator nl*
  private static boolean unary_not_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_not_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = not_operator(b, l + 1);
    r = r && unary_not_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean unary_not_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_not_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "unary_not_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // compare_operator nl*
  private static boolean compare_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compare_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = compare_operator(b, l + 1);
    r = r && compare_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean compare_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "compare_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "compare_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // plusminus_operator nl*
  private static boolean plusminus_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "plusminus_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = plusminus_operator(b, l + 1);
    r = r && plusminus_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean plusminus_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "plusminus_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "plusminus_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // muldiv_operator nl*
  private static boolean muldiv_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "muldiv_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = muldiv_operator(b, l + 1);
    r = r && muldiv_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean muldiv_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "muldiv_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "muldiv_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // infix_operator nl*
  private static boolean infix_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infix_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = infix_operator(b, l + 1);
    r = r && infix_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean infix_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "infix_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "infix_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // colon_operator nl*
  private static boolean colon_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "colon_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = colon_operator(b, l + 1);
    r = r && colon_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean colon_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "colon_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "colon_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  public static boolean unary_plusminus_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_plusminus_expression")) return false;
    if (!nextTokenIsSmart(b, R_PLUS, R_MINUS)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = unary_plusminus_expression_0(b, l + 1);
    p = r;
    r = p && expression(b, l, 22);
    exit_section_(b, l, m, R_OPERATOR_EXPRESSION, r, p, null);
    return r || p;
  }

  // plusminus_operator nl*
  private static boolean unary_plusminus_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_plusminus_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = plusminus_operator(b, l + 1);
    r = r && unary_plusminus_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean unary_plusminus_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "unary_plusminus_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "unary_plusminus_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // exp_operator nl*
  private static boolean exp_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = exp_operator(b, l + 1);
    r = r && exp_expression_0_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean exp_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "exp_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "exp_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '[' nl* ']' | '[' nl* subscription_expr_list ']' |
  //   '[[' nl* ']]' | '[[' nl* subscription_expr_list ']]'
  private static boolean subscription_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = subscription_expression_0_0(b, l + 1);
    if (!r) r = subscription_expression_0_1(b, l + 1);
    if (!r) r = subscription_expression_0_2(b, l + 1);
    if (!r) r = subscription_expression_0_3(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // '[' nl* ']'
  private static boolean subscription_expression_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_LBRACKET);
    r = r && subscription_expression_0_0_1(b, l + 1);
    r = r && consumeToken(b, R_RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean subscription_expression_0_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expression_0_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '[' nl* subscription_expr_list ']'
  private static boolean subscription_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_LBRACKET);
    r = r && subscription_expression_0_1_1(b, l + 1);
    r = r && subscription_expr_list(b, l + 1);
    r = r && consumeToken(b, R_RBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean subscription_expression_0_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_1_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expression_0_1_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '[[' nl* ']]'
  private static boolean subscription_expression_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_2")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_LDBRACKET);
    r = r && subscription_expression_0_2_1(b, l + 1);
    r = r && consumeToken(b, R_RDBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean subscription_expression_0_2_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_2_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expression_0_2_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '[[' nl* subscription_expr_list ']]'
  private static boolean subscription_expression_0_3(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_3")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_LDBRACKET);
    r = r && subscription_expression_0_3_1(b, l + 1);
    r = r && subscription_expr_list(b, l + 1);
    r = r && consumeToken(b, R_RDBRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean subscription_expression_0_3_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "subscription_expression_0_3_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "subscription_expression_0_3_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '$' nl* member_tag
  private static boolean member_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "member_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_LIST_SUBSET);
    r = r && member_expression_0_1(b, l + 1);
    r = r && member_tag(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean member_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "member_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "member_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '@' nl* member_tag
  private static boolean at_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "at_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_AT);
    r = r && at_expression_0_1(b, l + 1);
    r = r && member_tag(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // nl*
  private static boolean at_expression_0_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "at_expression_0_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!consumeTokenSmart(b, R_NL)) break;
      if (!empty_element_parsed_guard_(b, "at_expression_0_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // ('::' | ':::') identifier
  private static boolean namespace_access_expression_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "namespace_access_expression_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = namespace_access_expression_0_0(b, l + 1);
    r = r && consumeToken(b, R_IDENTIFIER);
    exit_section_(b, m, null, r);
    return r;
  }

  // '::' | ':::'
  private static boolean namespace_access_expression_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "namespace_access_expression_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_DOUBLECOLON);
    if (!r) r = consumeTokenSmart(b, R_TRIPLECOLON);
    exit_section_(b, m, null, r);
    return r;
  }

  // identifier | special_constant
  public static boolean reference_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "reference_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_REFERENCE_EXPRESSION, "<reference expression>");
    r = consumeTokenSmart(b, R_IDENTIFIER);
    if (!r) r = special_constant(b, l + 1);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // integer | numeric | complex
  public static boolean numeric_literal_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "numeric_literal_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_NUMERIC_LITERAL_EXPRESSION, "<numeric literal expression>");
    r = consumeTokenSmart(b, R_INTEGER);
    if (!r) r = consumeTokenSmart(b, R_NUMERIC);
    if (!r) r = consumeTokenSmart(b, R_COMPLEX);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // string
  public static boolean string_literal_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "string_literal_expression")) return false;
    if (!nextTokenIsSmart(b, R_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_STRING);
    exit_section_(b, m, R_STRING_LITERAL_EXPRESSION, r);
    return r;
  }

  // TRUE | FALSE | T | F
  public static boolean logical_literal_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "logical_literal_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_LOGICAL_LITERAL_EXPRESSION, "<logical literal expression>");
    r = consumeTokenSmart(b, R_TRUE);
    if (!r) r = consumeTokenSmart(b, R_FALSE);
    if (!r) r = consumeTokenSmart(b, R_T);
    if (!r) r = consumeTokenSmart(b, R_F);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

  // NULL
  public static boolean null_literal_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "null_literal_expression")) return false;
    if (!nextTokenIsSmart(b, R_NULL)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeTokenSmart(b, R_NULL);
    exit_section_(b, m, R_NULL_LITERAL_EXPRESSION, r);
    return r;
  }

  // NA | NA_INTEGER | NA_REAL | NA_COMPLEX | NA_CHARACTER
  public static boolean na_literal_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "na_literal_expression")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, R_NA_LITERAL_EXPRESSION, "<na literal expression>");
    r = consumeTokenSmart(b, R_NA);
    if (!r) r = consumeTokenSmart(b, R_NA_INTEGER);
    if (!r) r = consumeTokenSmart(b, R_NA_REAL);
    if (!r) r = consumeTokenSmart(b, R_NA_COMPLEX);
    if (!r) r = consumeTokenSmart(b, R_NA_CHARACTER);
    exit_section_(b, l, m, r, false, null);
    return r;
  }

}
