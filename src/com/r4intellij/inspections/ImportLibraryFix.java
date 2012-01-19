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
import com.r4intellij.psi.RCommand;
import com.r4intellij.psi.RFile;
import com.r4intellij.psi.RFuncall;
import com.r4intellij.psi.impl.RElementFactory;
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
            RFile file = (RFile) element.getContainingFile();
            List<RFuncall> importStatements = file.getImportStatements();

            PsiElement insertAfter = importStatements.size() > 0 ? importStatements.get(importStatements.size() - 1).getParent() : element.getContainingFile().getFirstChild();
            if (insertAfter == null) return;

            addNextRule(project, insertAfter, "library(" + packageName + ");");
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

    public static RCommand addNextRule(Project project, PsiElement inserAfter, String newRuleText) {
        RCommand addedImport = (RCommand) inserAfter.getParent().addAfter(RElementFactory.createFuncallFromText(project, newRuleText), inserAfter);
        inserAfter.getParent().addBefore(RElementFactory.createLeafFromText(project, "\n"), addedImport);
//        if (endsWithSemicolon(inserAfter)) {
//            addedImport.addBefore(BnfElementFactory.createLeafFromText(project, ";"), null);
//            if (inserAfter.getNextSibling() instanceof PsiWhiteSpace) {
//                inserAfter.getParent().addAfter(BnfElementFactory.createLeafFromText(project, "\n"), addedImport);
//            }
//        }
        return addedImport;
    }
}
