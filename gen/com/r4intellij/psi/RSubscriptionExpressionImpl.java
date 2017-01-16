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

public class RSubscriptionExpressionImpl extends RExpressionImpl implements RSubscriptionExpression {

  public RSubscriptionExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitSubscriptionExpression(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) accept((RVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<RExpression> getExpressionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, RExpression.class);
  }

  @Override
  @Nullable
  public PsiElement getLbracket() {
    return findChildByType(R_LBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getLdbracket() {
    return findChildByType(R_LDBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getRbracket() {
    return findChildByType(R_RBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getRdbracket() {
    return findChildByType(R_RDBRACKET);
  }

}
