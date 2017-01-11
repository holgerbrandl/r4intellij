package com.r4intellij.lexer;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.MergingLexerAdapter;
import com.intellij.psi.tree.TokenSet;
import com.r4intellij.parsing.RParserDefinition;

public class RLexer extends MergingLexerAdapter {
  private static final TokenSet TOKENS_TO_MERGE = TokenSet.create(RParserDefinition.SPACE);

  public RLexer() {
    super(new FlexAdapter(new _RLexer((java.io.Reader)null)), TOKENS_TO_MERGE);
  }
}
