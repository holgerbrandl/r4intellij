// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.stubs.IStubElementType;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RPsiElement;
import com.r4intellij.psi.api.RVisitor;
import com.r4intellij.psi.stubs.RAssignmentBase;
import org.jetbrains.annotations.NotNull;

public class RAssignmentStatementImpl extends RAssignmentBase implements RAssignmentStatement {

  public RAssignmentStatementImpl(ASTNode node) {
    super(node);
  }

  public RAssignmentStatementImpl(com.r4intellij.psi.stubs.RAssignmentStub stub, IStubElementType nodeType) {
    super(stub, nodeType);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) ((RVisitor)visitor).visitAssignmentStatement(this);
    else super.accept(visitor);
  }

  public boolean isLeft() {
    return RPsiImplUtil.isLeft(this);
  }

  public boolean isRight() {
    return RPsiImplUtil.isRight(this);
  }

  public boolean isEqual() {
    return RPsiImplUtil.isEqual(this);
  }

  public RPsiElement getAssignedValue() {
    return RPsiImplUtil.getAssignedValue(this);
  }

  public PsiElement getAssignee() {
    return RPsiImplUtil.getAssignee(this);
  }

  public String getName() {
    return RPsiImplUtil.getName(this);
  }

  public PsiElement setName(String name) {
    return RPsiImplUtil.setName(this, name);
  }

  public ASTNode getNameNode() {
    return RPsiImplUtil.getNameNode(this);
  }

}
