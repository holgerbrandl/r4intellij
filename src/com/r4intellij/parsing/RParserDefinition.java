package com.r4intellij.parsing;

import com.intellij.lang.ASTNode;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import com.r4intellij.lexer.RLexer;
import com.r4intellij.psi.RElementType;
import com.r4intellij.psi.RFileElementType;
import com.r4intellij.psi.RFileImpl;
import org.jetbrains.annotations.NotNull;

public class RParserDefinition implements ParserDefinition {
  private final TokenSet myWhitespaceTokens;
  private final TokenSet myCommentTokens;
  private final TokenSet myStringLiteralTokens;

  public static IFileElementType FILE = new RFileElementType();
  public static IElementType END_OF_LINE_COMMENT = new RElementType("END_OF_LINE_COMMENT");
  public static IElementType BAD_CHARACTER = new RElementType("BAD_CHARACTER");
  public static IElementType SPACE = new RElementType("SPACE");
  public static IElementType TAB = new RElementType("TAB");
  public static IElementType FORMFEED = new RElementType("FORMFEED");


  public RParserDefinition() {
    myWhitespaceTokens = TokenSet.create(SPACE, TAB, FORMFEED);
    myCommentTokens = TokenSet.create(END_OF_LINE_COMMENT);
    myStringLiteralTokens = TokenSet.create(RElementTypes.THE_R_STRING);
  }

  @Override
  @NotNull
  public Lexer createLexer(Project project) {
    return new RLexer();
  }

  @Override
  public IFileElementType getFileNodeType() {
    return FILE;
  }

  @Override
  @NotNull
  public TokenSet getWhitespaceTokens() {
    return myWhitespaceTokens;
  }

  @Override
  @NotNull
  public TokenSet getCommentTokens() {
    return myCommentTokens;
  }

  @Override
  @NotNull
  public TokenSet getStringLiteralElements() {
    return myStringLiteralTokens;
  }

  @Override
  @NotNull
  public PsiParser createParser(Project project) {
    return new RParser();
  }

  @Override
  @NotNull
  public PsiElement createElement(@NotNull ASTNode node) {
    return RElementTypes.Factory.createElement(node);
  }

  @Override
  public PsiFile createFile(FileViewProvider viewProvider) {
    return new RFileImpl(viewProvider);
  }

  @Override
  public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right) {
    return SpaceRequirements.MAY;
  }
}
