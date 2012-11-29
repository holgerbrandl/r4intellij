/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.r4intellij.psi.RFuncall;

import java.util.ArrayList;
import java.util.List;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class RPsiUtils {

    public static PsiElement getPsiFromText(String text, Project project) {
        return PsiFileFactory.getInstance(project).createFileFromText("a.R", text).getFirstChild();
    }


    public static List<RFuncall> collectLibraryStatements(PsiFile containingFile) {
        final List<RFuncall> libStatements = new ArrayList<RFuncall>();

        containingFile.accept(new PsiElementVisitor() {
            @Override
            public void visitElement(PsiElement element) {
                if (element instanceof RFuncall && ((RFuncall) element).getVariable().getText().equals("library")) {
                    libStatements.add((RFuncall) element);
                }

                element.acceptChildren(this);
            }
        });

        return libStatements;  //To change body of created methods use File | Settings | File Templates.
    }
}
