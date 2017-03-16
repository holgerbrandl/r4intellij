package com.r4intellij.psi;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RLanguage;
import com.r4intellij.psi.api.RCallExpression;


/**
 * @author brandl
 */
public class RElementFactory {

    private RElementFactory() {
    }


    public static PsiElement createLeafFromText(Project project, String text) {
        PsiFile fileFromText = buildRFileFromText(project, text);
        return PsiTreeUtil.getDeepestFirst(fileFromText);
    }


    public static PsiFile buildRFileFromText(Project project, String text) {
        return PsiFileFactory.getInstance(project).createFileFromText("a.R", RLanguage.getInstance(), text);
    }


    //
    public static RCallExpression createFuncallFromText(Project project, String text) {
        PsiFile fromText = buildRFileFromText(project, text);
//        return (RCallExpression) fromText.getFirstChild().getChildren()[0];
        return (RCallExpression) fromText.getFirstChild();
    }

}
