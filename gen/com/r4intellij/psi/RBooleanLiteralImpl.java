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

public class RBooleanLiteralImpl extends RElementImpl implements RBooleanLiteral {

  public RBooleanLiteralImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitBooleanLiteral(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) accept((RVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getFalse() {
    return findChildByType(R_FALSE);
  }

  @Override
  @Nullable
  public PsiElement getTrue() {
    return findChildByType(R_TRUE);
  }

  public boolean isTrue() {
    return RPsiImplUtil.isTrue(this);
  }

  public boolean isFalse() {
    return RPsiImplUtil.isFalse(this);
  }

}
