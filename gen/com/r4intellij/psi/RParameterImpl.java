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

public class RParameterImpl extends RElementImpl implements RParameter {

  public RParameterImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitParameter(this);
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
  public PsiElement getEq() {
    return findChildByType(R_EQ);
  }

  @Override
  @Nullable
  public PsiElement getIdentifier() {
    return findChildByType(R_IDENTIFIER);
  }

  public ASTNode getNameNode() {
    return RPsiImplUtil.getNameNode(this);
  }

  public String getName() {
    return RPsiImplUtil.getName(this);
  }

  public PsiElement setName(String name) {
    return RPsiImplUtil.setName(this, name);
  }

}
