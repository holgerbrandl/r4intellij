package com.r4intellij.psi;

import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.StubElement;
import org.jetbrains.annotations.NotNull;

public class RElementImpl extends RBaseElementImpl<StubElement> {
  public RElementImpl(@NotNull final ASTNode astNode) {
    super(astNode);
  }
}
