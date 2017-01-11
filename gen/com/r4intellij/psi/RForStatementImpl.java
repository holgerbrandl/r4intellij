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

public class RForStatementImpl extends RExpressionImpl implements RForStatement {

  public RForStatementImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitForStatement(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<RExpression> getExpressionList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, RExpression.class);
  }

  @Override
  @NotNull
  public PsiElement getLpar() {
    return findNotNullChildByType(THE_R_LPAR);
  }

  @Override
  @NotNull
  public PsiElement getRpar() {
    return findNotNullChildByType(THE_R_RPAR);
  }

  @Override
  @NotNull
  public PsiElement getFor() {
    return findNotNullChildByType(THE_R_FOR);
  }

  @Override
  @NotNull
  public RExpression getTarget() {
    List<RExpression> p1 = getExpressionList();
    return p1.get(0);
  }

  @Override
  @Nullable
  public RExpression getRange() {
    List<RExpression> p1 = getExpressionList();
    return p1.size() < 2 ? null : p1.get(1);
  }

  @Override
  @Nullable
  public RExpression getBody() {
    List<RExpression> p1 = getExpressionList();
    return p1.size() < 3 ? null : p1.get(2);
  }

}
