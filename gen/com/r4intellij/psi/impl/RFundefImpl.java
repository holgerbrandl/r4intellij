/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.r4intellij.psi.RTypes.*;
import com.r4intellij.psi.*;

public class RFundefImpl extends RCompositeElementImpl implements RFundef {

  public RFundefImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitFundef(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public RExprOrAssign getExprOrAssign() {
    return findNotNullChildByClass(RExprOrAssign.class);
  }

  @Override
  @Nullable
  public RFundefArgs getFundefArgs() {
    return findChildByClass(RFundefArgs.class);
  }

}
