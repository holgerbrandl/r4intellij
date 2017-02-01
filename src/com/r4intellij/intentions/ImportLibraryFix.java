package com.r4intellij.intentions;

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


    @SuppressWarnings("WeakerAccess")
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
        return "Dependency management";
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
//                insertAfter = PsiTreeUtil.getContextOfType(importStatements.get(importStatements.size() - 1), RExpression.class);
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

            // move around the caret
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
}
