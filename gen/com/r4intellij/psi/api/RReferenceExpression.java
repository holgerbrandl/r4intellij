// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.r4intellij.psi.references.RReferenceImpl;

public interface RReferenceExpression extends RExpression {

  @NotNull
  PsiElement getIdentifier();

  RReferenceImpl getReference();

  @Nullable
  String getNamespace();

  String getName();

}
