// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.r4intellij.typing.types.RType;

public interface RNaLiteralExpression extends RExpression {

  @Nullable
  PsiElement getNa();

  @Nullable
  PsiElement getNaCharacter();

  @Nullable
  PsiElement getNaComplex();

  @Nullable
  PsiElement getNaInteger();

  @Nullable
  PsiElement getNaReal();

  RType getType();

}
