// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.r4intellij.parsing.RElementTypes.*;
import com.r4intellij.psi.stubs.RAssignmentBase;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.stubs.RAssignmentStub;
import com.intellij.psi.stubs.IStubElementType;

public class RAssignmentStatementImpl extends RAssignmentBase implements RAssignmentStatement {

  public RAssignmentStatementImpl(ASTNode node) {
    super(node);
  }

  public RAssignmentStatementImpl(RAssignmentStub stub, IStubElementType type) {
    super(stub, type);
  }

  public void accept(@NotNull RVisitor visitor) {
    visitor.visitAssignmentStatement(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof RVisitor) accept((RVisitor)visitor);
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
