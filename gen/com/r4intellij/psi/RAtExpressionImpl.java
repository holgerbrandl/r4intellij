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

public class RAtExpressionImpl extends RExpressionImpl implements RAtExpression {

  public RAtExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitAtExpression(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public RExpression getExpression() {
    return findNotNullChildByClass(RExpression.class);
  }

  @Override
  @NotNull
  public PsiElement getAt() {
    return findNotNullChildByType(THE_R_AT);
  }

  @Override
  @Nullable
  public PsiElement getIdentifier() {
    return findChildByType(THE_R_IDENTIFIER);
  }

  @Override
  @Nullable
  public PsiElement getString() {
    return findChildByType(THE_R_STRING);
  }

  public String getTag() {
    return RPsiImplUtil.getTag(this);
  }

}
