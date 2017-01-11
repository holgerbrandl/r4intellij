package com.r4intellij.highlighting;

import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

public class RSyntaxHighlighterColors {
  public static final String R_KEYWORD = "R_KEYWORD";
  private static final String R_LINE_COMMENT = "R_LINE_COMMENT";

  private static final String R_NUMBER = "R_NUMBER";
  private static final String R_STRING = "R_STRING";
  private static final String R_OPERATION_SIGN = "R_OPERATION_SIGN";
  private static final String R_PARENTH = "R_PARENTH";
  private static final String R_BRACKETS = "R_BRACKETS";
  private static final String R_BRACES = "R_BRACES";
  private static final String R_COMMA = "R_COMMA";
  private static final String R_SEMICOLON = "R_SEMICOLON";
  private static final String R_BAD_CHARACTER = "R_BAD_CHARACTER";

  public static final TextAttributesKey LINE_COMMENT =
    createTextAttributesKey(R_LINE_COMMENT, DefaultLanguageHighlighterColors.LINE_COMMENT);
  public static final TextAttributesKey KEYWORD =
    createTextAttributesKey(R_KEYWORD, DefaultLanguageHighlighterColors.KEYWORD);
  public static final TextAttributesKey NUMBER =
    createTextAttributesKey(R_NUMBER, DefaultLanguageHighlighterColors.NUMBER);
  public static final TextAttributesKey STRING =
    createTextAttributesKey(R_STRING, DefaultLanguageHighlighterColors.STRING);
  public static final TextAttributesKey OPERATION_SIGN =
    createTextAttributesKey(R_OPERATION_SIGN, DefaultLanguageHighlighterColors.OPERATION_SIGN);
  public static final TextAttributesKey PARENTHS =
    createTextAttributesKey(R_PARENTH, DefaultLanguageHighlighterColors.PARENTHESES);
  public static final TextAttributesKey BRACKETS =
    createTextAttributesKey(R_BRACKETS, DefaultLanguageHighlighterColors.BRACKETS);
  public static final TextAttributesKey BRACES =
    createTextAttributesKey(R_BRACES, DefaultLanguageHighlighterColors.BRACES);
  public static final TextAttributesKey COMMA = createTextAttributesKey(R_COMMA, DefaultLanguageHighlighterColors.COMMA);
  public static final TextAttributesKey SEMICOLON =
    createTextAttributesKey(R_SEMICOLON, DefaultLanguageHighlighterColors.SEMICOLON);
  public static final TextAttributesKey BAD_CHARACTER =
    createTextAttributesKey(R_BAD_CHARACTER, HighlighterColors.BAD_CHARACTER);
}
