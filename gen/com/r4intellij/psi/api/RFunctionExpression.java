// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface RFunctionExpression extends RExpression {

  @Nullable
  RExpression getExpression();

  @NotNull
  RParameterList getParameterList();

  @NotNull
  PsiElement getFunction();

  @Nullable
  String getDocStringValue();

}
