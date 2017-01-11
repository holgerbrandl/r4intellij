// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface RNextStatement extends RExpression {

  @Nullable
  RExpression getExpression();

  @Nullable
  PsiElement getLpar();

  @Nullable
  PsiElement getRpar();

  @NotNull
  PsiElement getNext();

}
