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

public class RHelpExpressionImpl extends RExpressionImpl implements RHelpExpression {

  public RHelpExpressionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitHelpExpression(this);
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
  public PsiElement getNaCharacter() {
    return findChildByType(R_NA_CHARACTER);
  }

  @Override
  @Nullable
  public PsiElement getNaComplex() {
    return findChildByType(R_NA_COMPLEX);
  }

  @Override
  @Nullable
  public PsiElement getNaInteger() {
    return findChildByType(R_NA_INTEGER);
  }

  @Override
  @Nullable
  public PsiElement getNaReal() {
    return findChildByType(R_NA_REAL);
  }

  @Override
  @Nullable
  public PsiElement getTripleDots() {
    return findChildByType(R_TRIPLE_DOTS);
  }

  @Override
  @Nullable
  public PsiElement getBreak() {
    return findChildByType(R_BREAK);
  }

  @Override
  @Nullable
  public PsiElement getElse() {
    return findChildByType(R_ELSE);
  }

  @Override
  @Nullable
  public PsiElement getFor() {
    return findChildByType(R_FOR);
  }

  @Override
  @Nullable
  public PsiElement getFunction() {
    return findChildByType(R_FUNCTION);
  }

  @Override
  @Nullable
  public PsiElement getIf() {
    return findChildByType(R_IF);
  }

  @Override
  @Nullable
  public PsiElement getIn() {
    return findChildByType(R_IN);
  }

  @Override
  @Nullable
  public PsiElement getNext() {
    return findChildByType(R_NEXT);
  }

  @Override
  @Nullable
  public PsiElement getRepeat() {
    return findChildByType(R_REPEAT);
  }

  @Override
  @Nullable
  public PsiElement getWhile() {
    return findChildByType(R_WHILE);
  }

}
