// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RParameterList;
import com.r4intellij.psi.api.RVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.r4intellij.parsing.RElementTypes.THE_R_FUNCTION;

public class RFunctionExpressionImpl extends RExpressionImpl implements RFunctionExpression {

  public RFunctionExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitFunctionExpression(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public RExpression getExpression() {
    return findChildByClass(RExpression.class);
  }

  @Override
  @NotNull
  public RParameterList getParameterList() {
    return findNotNullChildByClass(RParameterList.class);
  }

  @Override
  @NotNull
  public PsiElement getFunction() {
    return findNotNullChildByType(THE_R_FUNCTION);
  }

  @Nullable
  public String getDocStringValue() {
    return RPsiImplUtil.getDocStringValue(this);
  }

}
