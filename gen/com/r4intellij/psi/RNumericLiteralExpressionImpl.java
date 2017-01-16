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

public class RNumericLiteralExpressionImpl extends RExpressionImpl implements RNumericLiteralExpression {

  public RNumericLiteralExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitNumericLiteralExpression(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) accept((RVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getComplex() {
    return findChildByType(R_COMPLEX);
  }

  @Override
  @Nullable
  public PsiElement getInteger() {
    return findChildByType(R_INTEGER);
  }

  @Override
  @Nullable
  public PsiElement getNumeric() {
    return findChildByType(R_NUMERIC);
  }

}
