package com.r4intellij.refactoring;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.lang.Language;
import com.intellij.lang.refactoring.InlineActionHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.r4intellij.RLanguage;
import com.r4intellij.psi.api.RAssignmentStatement;

import java.util.Collection;

/**
 * In IJ codebase there seems to be a different inline impls for methods, parameters, consts etc.
 * <p>
 * Best too to complex  example org.intellij.grammar.refactor.BnfInlineRuleActionHandler
 *
 * @author Holger Brandl
 */
public class RInlineActionHandler extends InlineActionHandler {
    @Override
    public boolean isEnabledForLanguage(Language language) {
        return language.equals(RLanguage.getInstance());
    }


    @Override
    public boolean canInlineElement(PsiElement psiElement) {
        return psiElement instanceof RAssignmentStatement;
//        return true;

        // what about PsiUtil.isLocalVariable(element)
    }


    @Override
    public void inlineElement(Project project, Editor editor, PsiElement psiElement) {
        // nice example see community: org.jetbrains.plugins.groovy.refactoring.inline.GroovyInlineLocalHandler
        // best example org.intellij.grammar.refactor.BnfInlineRuleActionHandler
//        CommonRefactoringUtil.showErrorHint(project, editor, "Cool Rule has errors", "Inline Rule", null);

        if (PsiTreeUtil.hasErrorElements(psiElement)) {
            CommonRefactoringUtil.showErrorHint(project, editor, "Rule has errors", "Inline Rule", null);
            return;
        }

        Collection<PsiReference> allReferences = ReferencesSearch.search(psiElement).findAll();
        if (allReferences.isEmpty()) {
            CommonRefactoringUtil.showErrorHint(project, editor, "Rule is never used", "Inline Rule", null);
            return;
        }

        if (!CommonRefactoringUtil.checkReadOnlyStatus(project, psiElement)) return;

        // needed to allow for for current caret element only inlining
        PsiReference reference = editor != null ? TargetElementUtil.findReference(editor, editor.getCaretModel().getOffset()) : null;
        if (reference != null && !psiElement.equals(reference.resolve())) {
            reference = null;
        }

        InlineAssignmentDialog dialog = new InlineAssignmentDialog(project, (RAssignmentStatement) psiElement, reference);
        dialog.show();
    }
}
