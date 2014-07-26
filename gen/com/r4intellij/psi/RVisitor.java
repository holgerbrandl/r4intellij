/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi;

import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElementVisitor;

public class RVisitor extends PsiElementVisitor {

  public void visitCommand(@NotNull RCommand o) {
    visitCompositeElement(o);
  }

  public void visitCond(@NotNull RCond o) {
    visitCompositeElement(o);
  }

  public void visitDocument(@NotNull RDocument o) {
    visitCompositeElement(o);
  }

  public void visitExpr(@NotNull RExpr o) {
    visitCompositeElement(o);
  }

  public void visitExprOrAssign(@NotNull RExprOrAssign o) {
    visitCompositeElement(o);
  }

  public void visitExprlist(@NotNull RExprlist o) {
    visitCompositeElement(o);
  }

  public void visitFdArgument(@NotNull RFdArgument o) {
    visitCompositeElement(o);
  }

  public void visitForcond(@NotNull RForcond o) {
    visitCompositeElement(o);
  }

  public void visitFuncall(@NotNull RFuncall o) {
    visitNamedElement(o);
  }

  public void visitFundef(@NotNull RFundef o) {
    visitCompositeElement(o);
  }

  public void visitFundefArgs(@NotNull RFundefArgs o) {
    visitCompositeElement(o);
  }

  public void visitSection(@NotNull RSection o) {
    visitCompositeElement(o);
  }

  public void visitStringLiteral(@NotNull RStringLiteral o) {
    visitCompositeElement(o);
  }

  public void visitSub(@NotNull RSub o) {
    visitCompositeElement(o);
  }

  public void visitSublist(@NotNull RSublist o) {
    visitCompositeElement(o);
  }

  public void visitVariable(@NotNull RVariable o) {
    visitCompositeElement(o);
  }

  public void visitNamedElement(@NotNull RNamedElement o) {
    visitCompositeElement(o);
  }

  public void visitCompositeElement(@NotNull RCompositeElement o) {
    visitElement(o);
  }

}
