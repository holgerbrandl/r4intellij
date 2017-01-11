// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface RMemberExpression extends RExpression {

  @NotNull
  RExpression getExpression();

  @NotNull
  PsiElement getListSubset();

  @Nullable
  PsiElement getIdentifier();

  @Nullable
  PsiElement getString();

  String getTag();

}
