// This is a generated file. Not intended for manual editing.
package com.r4intellij.psi.api;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import com.r4intellij.psi.stubs.RAssignmentStub;
import com.intellij.lang.ASTNode;

public interface RAssignmentStatement extends RNamedElement, StubBasedPsiElement<RAssignmentStub> {

  boolean isLeft();

  boolean isRight();

  boolean isEqual();

  RPsiElement getAssignedValue();

  PsiElement getAssignee();

  String getName();

  PsiElement setName(String name);

  ASTNode getNameNode();

}
