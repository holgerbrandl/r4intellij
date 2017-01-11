// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.r4intellij.parsing.RElementTypes.*;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.references.ROperatorReference;

public class ROperatorImpl extends RElementImpl implements ROperator {

  public ROperatorImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitOperator(this);
    else super.accept(visitor);
  }

  public String getName() {
    return RPsiImplUtil.getName(this);
  }

  public ROperatorReference getReference() {
    return RPsiImplUtil.getReference(this);
  }

}
