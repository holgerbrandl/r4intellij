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

public class RStringLiteralExpressionImpl extends RStringInjectHost implements RStringLiteralExpression {

  public RStringLiteralExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitStringLiteralExpression(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) accept((RVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getString() {
    return notNullChild(findChildByType(R_STRING));
  }

}
