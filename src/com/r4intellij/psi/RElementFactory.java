/*
 * Copyright 2011-2011 Gregory Shrago
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
        PsiFile fileFromText = PsiFileFactory.getInstance(project).createFileFromText("a.R", RLanguage.getInstance(), text);
        return PsiTreeUtil.getDeepestFirst(fileFromText);
    }


//    public static RStringLiteralExpression createExpressionFromText(Project project, String text) {
////        PsiFile fromText = PsiFileFactory.getInstance(project).createFileFromText("a.R", "\"" + text + "\";");
//        PsiFile fromText = PsiFileFactory.getInstance(project).createFileFromText("a.R", RLanguage.getInstance(), text + ";");
//        if ((fromText.getFirstChild()) != null) {
////            return (RStringLiteralExpression) ((RExpression) fromText.getFirstChild()).getExprOrAssign().getExpr().getStringLiteral();
//        }
//        return null;
//    }


    //
    public static RCallExpression createFuncallFromText(Project project, String text) {
        PsiFile fromText = PsiFileFactory.getInstance(project).createFileFromText("a.R", RLanguage.getInstance(), text);
        return (RCallExpression) fromText.getFirstChild().getChildren()[0];
    }

}
