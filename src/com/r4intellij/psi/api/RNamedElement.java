package com.r4intellij.psi.api;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiNamedElement;
import org.jetbrains.annotations.Nullable;

/**
 * @author Alefas
 * @since 27/01/15.
 */
public interface RNamedElement extends RPsiElement, PsiNamedElement {
  @Nullable
  ASTNode getNameNode();
}
