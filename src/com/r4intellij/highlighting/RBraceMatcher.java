package com.r4intellij.highlighting;

import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.r4intellij.parsing.RElementTypes;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class RBraceMatcher implements PairedBraceMatcher {
  private final BracePair[] PAIRS = new BracePair[]{
          new BracePair(RElementTypes.R_LPAR, RElementTypes.R_RPAR, false),
          new BracePair(RElementTypes.R_LBRACKET, RElementTypes.R_RBRACKET, false),
          new BracePair(RElementTypes.R_LDBRACKET, RElementTypes.R_RDBRACKET, false),
          new BracePair(RElementTypes.R_LBRACE, RElementTypes.R_RBRACE, false)};

  @Override
  public BracePair[] getPairs() {
    return PAIRS;
  }

  @Override
  public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType contextType) {
    return true;
  }

  @Override
  public int getCodeConstructStart(final PsiFile file, int openingBraceOffset) {
    return openingBraceOffset;
  }
}
