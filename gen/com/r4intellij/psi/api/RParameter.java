// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.lang.ASTNode;

public interface RParameter extends RNamedElement {

  @Nullable
  RExpression getExpression();

  @Nullable
  PsiElement getEq();

  @Nullable
  PsiElement getIdentifier();

  ASTNode getNameNode();

  String getName();

  PsiElement setName(String name);

}
