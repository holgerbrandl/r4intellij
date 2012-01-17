/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class RPsiUtils {

    public static PsiElement getPsiFromText(String text, Project project) {
        return PsiFileFactory.getInstance(project).createFileFromText("a.R", text).getFirstChild();
    }
}
