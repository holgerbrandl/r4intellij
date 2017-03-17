package com.r4intellij.psi.api;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

import java.util.List;

public interface RFile extends PsiFile {
    List<RCallExpression> getImportExpressions(PsiElement element);


    List<String> getImportedPackages(PsiElement element);
}
