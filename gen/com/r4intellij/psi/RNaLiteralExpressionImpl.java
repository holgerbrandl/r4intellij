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
import com.r4intellij.typing.types.RType;

public class RNaLiteralExpressionImpl extends RExpressionImpl implements RNaLiteralExpression {

  public RNaLiteralExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitNaLiteralExpression(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getNa() {
    return findChildByType(THE_R_NA);
  }

  @Override
  @Nullable
  public PsiElement getNaCharacter() {
    return findChildByType(THE_R_NA_CHARACTER);
  }

  @Override
  @Nullable
  public PsiElement getNaComplex() {
    return findChildByType(THE_R_NA_COMPLEX);
  }

  @Override
  @Nullable
  public PsiElement getNaInteger() {
    return findChildByType(THE_R_NA_INTEGER);
  }

  @Override
  @Nullable
  public PsiElement getNaReal() {
    return findChildByType(THE_R_NA_REAL);
  }

  public RType getType() {
    return RPsiImplUtil.getType(this);
  }

}
