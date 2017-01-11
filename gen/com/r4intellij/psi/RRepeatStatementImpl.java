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

public class RRepeatStatementImpl extends RExpressionImpl implements RRepeatStatement {

  public RRepeatStatementImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitRepeatStatement(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public RExpression getExpression() {
    return findChildByClass(RExpression.class);
  }

  @Override
  @NotNull
  public PsiElement getRepeat() {
    return findNotNullChildByType(THE_R_REPEAT);
  }

}
