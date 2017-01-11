// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElementVisitor;

public class RVisitor extends PsiElementVisitor {

  public void visitArgumentList(@NotNull RArgumentList o) {
    visitPsiElement(o);
  }

  public void visitAssignmentStatement(@NotNull RAssignmentStatement o) {
    visitNamedElement(o);
  }

  public void visitAtExpression(@NotNull RAtExpression o) {
    visitExpression(o);
  }

  public void visitBlockExpression(@NotNull RBlockExpression o) {
    visitExpression(o);
  }

  public void visitBreakStatement(@NotNull RBreakStatement o) {
    visitExpression(o);
  }

  public void visitCallExpression(@NotNull RCallExpression o) {
    visitExpression(o);
  }

  public void visitEmptyExpression(@NotNull REmptyExpression o) {
    visitExpression(o);
  }

  public void visitExpression(@NotNull RExpression o) {
    visitPsiElement(o);
  }

  public void visitForStatement(@NotNull RForStatement o) {
    visitExpression(o);
  }

  public void visitFunctionExpression(@NotNull RFunctionExpression o) {
    visitExpression(o);
  }

  public void visitHelpExpression(@NotNull RHelpExpression o) {
    visitExpression(o);
  }

  public void visitIfStatement(@NotNull RIfStatement o) {
    visitExpression(o);
  }

  public void visitLogicalLiteralExpression(@NotNull RLogicalLiteralExpression o) {
    visitExpression(o);
  }

  public void visitMemberExpression(@NotNull RMemberExpression o) {
    visitExpression(o);
  }

  public void visitNaLiteralExpression(@NotNull RNaLiteralExpression o) {
    visitExpression(o);
  }

  public void visitNextStatement(@NotNull RNextStatement o) {
    visitExpression(o);
  }

  public void visitNullLiteralExpression(@NotNull RNullLiteralExpression o) {
    visitExpression(o);
  }

  public void visitNumericLiteralExpression(@NotNull RNumericLiteralExpression o) {
    visitExpression(o);
  }

  public void visitOperator(@NotNull ROperator o) {
    visitPsiElement(o);
  }

  public void visitOperatorExpression(@NotNull ROperatorExpression o) {
    visitExpression(o);
  }

  public void visitParameter(@NotNull RParameter o) {
    visitNamedElement(o);
  }

  public void visitParameterList(@NotNull RParameterList o) {
    visitPsiElement(o);
  }

  public void visitParenthesizedExpression(@NotNull RParenthesizedExpression o) {
    visitExpression(o);
  }

  public void visitReferenceExpression(@NotNull RReferenceExpression o) {
    visitExpression(o);
  }

  public void visitRepeatStatement(@NotNull RRepeatStatement o) {
    visitExpression(o);
  }

  public void visitSliceExpression(@NotNull RSliceExpression o) {
    visitExpression(o);
  }

  public void visitStringLiteralExpression(@NotNull RStringLiteralExpression o) {
    visitExpression(o);
  }

  public void visitSubscriptionExpression(@NotNull RSubscriptionExpression o) {
    visitExpression(o);
  }

  public void visitTildeExpression(@NotNull RTildeExpression o) {
    visitExpression(o);
  }

  public void visitUnaryTildeExpression(@NotNull RUnaryTildeExpression o) {
    visitExpression(o);
  }

  public void visitWhileStatement(@NotNull RWhileStatement o) {
    visitExpression(o);
  }

  public void visitNamedElement(@NotNull RNamedElement o) {
    visitPsiElement(o);
  }

  public void visitPsiElement(@NotNull RPsiElement o) {
    visitElement(o);
  }

}
