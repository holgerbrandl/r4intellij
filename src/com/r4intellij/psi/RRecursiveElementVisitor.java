package com.r4intellij.psi;

import com.intellij.psi.PsiElement;
import com.r4intellij.psi.api.RVisitor;

public class RRecursiveElementVisitor extends RVisitor {
    @Override
    public void visitElement(PsiElement element) {
        element.acceptChildren(this);
    }
}
