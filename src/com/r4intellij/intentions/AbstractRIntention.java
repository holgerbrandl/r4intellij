/*
 * Copyright 2000-2016 JetBrains s.r.o.
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
package com.r4intellij.intentions;

import com.intellij.codeInsight.CodeInsightUtilCore;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import com.r4intellij.RLanguage;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;


public abstract class AbstractRIntention implements IntentionAction {
    private final PsiElementPredicate predicate;


    protected AbstractRIntention() {
        super();
        predicate = getElementPredicate();
    }


    protected abstract void processIntention(@NotNull PsiElement element, @NotNull Project project, Editor editor) throws IncorrectOperationException;


    @NotNull
    protected abstract PsiElementPredicate getElementPredicate();


    @Override
    public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
        final PsiElement element = findMatchingElement(file, editor);
        if (element == null) {
            return;
        }
        assert element.isValid() : element;
        processIntention(element, project, editor);
    }


    @Nullable
    PsiElement findMatchingElement(PsiFile file, Editor editor) {
        if (!file.getViewProvider().getLanguages().contains(RLanguage.getInstance())) {
            return null;
        }

        SelectionModel selectionModel = editor.getSelectionModel();
        if (selectionModel.hasSelection()) {
            int start = selectionModel.getSelectionStart();
            int end = selectionModel.getSelectionEnd();

            if (0 <= start && start <= end) {
                TextRange selectionRange = new TextRange(start, end);
                PsiElement element = CodeInsightUtilCore.findElementInRange(file, start, end, PsiElement.class, RLanguage.getInstance());
                while (element != null && element.getTextRange() != null && selectionRange.contains(element.getTextRange())) {
                    if (predicate.satisfiedBy(element)) return element;
                    element = element.getParent();
                }
            }
        }

        final int position = editor.getCaretModel().getOffset();
        PsiElement element = file.findElementAt(position);
        while (element != null) {
            if (predicate.satisfiedBy(element)) return element;
            if (isStopElement(element)) break;
            element = element.getParent();
        }

        element = file.findElementAt(position - 1);
        while (element != null) {
            if (predicate.satisfiedBy(element)) return element;
            if (isStopElement(element)) return null;
            element = element.getParent();
        }

        return null;
    }


    @Contract(value = "null -> false", pure = true)
    private boolean isStopElement(PsiElement element) {
        return element instanceof PsiFile;
    }


    @Override
    public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
        return findMatchingElement(file, editor) != null;
    }


    @Override
    public boolean startInWriteAction() {
        return true;
    }
}
