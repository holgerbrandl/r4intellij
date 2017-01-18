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

package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.AccessToken;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.r4intellij.psi.RElementFactory;
import com.r4intellij.psi.RFileImpl;
import com.r4intellij.psi.api.RCallExpression;
import org.jetbrains.annotations.NotNull;

import java.util.List;


/**
 * @author Holger Brandl
 */
public class ImportLibraryFix implements LocalQuickFix {

    private final String packageName;


    public ImportLibraryFix(String name) {
        packageName = name;
    }


    @NotNull
    @Override
    public String getName() {
        return "Import '" + packageName + "'";
    }


    @NotNull
    @Override
    public String getFamilyName() {
        return "Create rule from usage";
    }


    @Override
    public void applyFix(final @NotNull Project project, @NotNull ProblemDescriptor descriptor) {
        final AccessToken token = WriteAction.start();
        try {
            final PsiElement element = descriptor.getPsiElement();
//            final BnfRule insertAfter = PsiTreeUtil.getParentOfType(element, BnfRule.class);
            RFileImpl file = (RFileImpl) element.getContainingFile();
            List<RCallExpression> importStatements = file.getPckgImportExpressions();

            PsiElement insertAfter = null;
            if (importStatements.size() > 0) {
//                insertAfter = getCommandParent(importStatements.get(importStatements.size() - 1).getParent());
//                insertAfter = PsiTreeUtil.getContextOfType(importStatements.get(importStatements.size() - 1), RExpression.class);
                // todo why not just
                insertAfter = importStatements.get(importStatements.size() - 1);
            }


            PsiElement importStatement = RElementFactory.createFuncallFromText(project, "library(" + packageName + ");");

            if (insertAfter != null && insertAfter.getTextOffset() < element.getTextOffset()) {
                importStatement = insertAfter.getParent().addAfter(importStatement, insertAfter);
                insertAfter.getParent().addBefore(RElementFactory.createLeafFromText(project, "\n"), importStatement);
            } else {
                insertAfter = file.getFirstChild();
                importStatement = insertAfter.getParent().addBefore(importStatement, insertAfter);
                insertAfter.getParent().addAfter(RElementFactory.createLeafFromText(project, "\n"), importStatement);
            }

//        if (endsWithSemicolon(inserAfter)) {
//            addedImport.addBefore(BnfElementFactory.createLeafFromText(project, ";"), null);
//            if (inserAfter.getNextSibling() instanceof PsiWhiteSpace) {
//                inserAfter.getParent().addAfter(BnfElementFactory.createLeafFromText(project, "\n"), addedImport);
//            }
//        }
            //            final FileEditor selectedEditor = FileEditorManager.getInstance(project).getSelectedEditor(insertAfter.getContainingFile().getVirtualFile());
//            if (selectedEditor instanceof TextEditor) {
//                final Editor editor = ((TextEditor) selectedEditor).getEditor();
//                editor.getCaretModel().moveToOffset(addedRule.getTextRange().getEndOffset() - (BnfIntroduceRuleHandler.endsWithSemicolon(addedRule) ? 1 : 0));
//                editor.getScrollingModel().scrollToCaret(ScrollType.MAKE_VISIBLE);
//            }
        } finally {
            token.finish();
        }
    }


//    private PsiElement getCommandParent(PsiElement psiElement) {
//        while (psiElement != null && !(psiElement instanceof RCommand))
//            psiElement = psiElement.getParent();
//
//        return psiElement;
//    }

}
