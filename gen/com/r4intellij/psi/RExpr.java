/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface RExpr extends RCompositeElement {

  @Nullable
  RCond getCond();

  @NotNull
  List<RExpr> getExprList();

  @Nullable
  RExprOrAssign getExprOrAssign();

  @Nullable
  RExprlist getExprlist();

  @Nullable
  RForcond getForcond();

  @Nullable
  RFuncall getFuncall();

  @Nullable
  RFundef getFundef();

  @Nullable
  RStringLiteral getStringLiteral();

  @NotNull
  List<RSublist> getSublistList();

  @Nullable
  RVariable getVariable();

}
