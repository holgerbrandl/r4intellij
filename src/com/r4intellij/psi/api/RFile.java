package com.r4intellij.psi.api;

import com.intellij.psi.PsiFile;

import java.util.List;

public interface RFile extends PsiFile {
    List<RFunctionExpression> getPckgImportExpressions();


    List<String> getImportedPackages();
}
