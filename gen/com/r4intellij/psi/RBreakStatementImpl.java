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

public class RBreakStatementImpl extends RExpressionImpl implements RBreakStatement {

  public RBreakStatementImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitBreakStatement(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) accept((RVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public RExpression getExpression() {
    return PsiTreeUtil.getChildOfType(this, RExpression.class);
  }

  @Override
  @Nullable
  public PsiElement getLpar() {
    return findChildByType(R_LPAR);
  }

  @Override
  @Nullable
  public PsiElement getRpar() {
    return findChildByType(R_RPAR);
  }

  @Override
  @NotNull
  public PsiElement getBreak() {
    return notNullChild(findChildByType(R_BREAK));
  }

}
