// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface RAtExpression extends RExpression {

  @NotNull
  RExpression getExpression();

  @NotNull
  PsiElement getAt();

  @Nullable
  PsiElement getIdentifier();

  @Nullable
  PsiElement getString();

  String getTag();

}
