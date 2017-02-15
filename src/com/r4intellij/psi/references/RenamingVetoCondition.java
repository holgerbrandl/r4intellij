package com.r4intellij.psi.references;

import com.intellij.openapi.util.Condition;
import com.intellij.psi.PsiElement;
import com.r4intellij.psi.api.RPsiElement;

import static com.r4intellij.documentation.RDocumentationProvider.isLibraryElement;

/**
 * Prevent renaming of library methods
 * <p>
 * https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000062144-How-to-prevent-renaming-of-library-functions-
 *
 * @author Holger Brandl
 */
public class RenamingVetoCondition implements Condition<PsiElement> {

    @Override
    public boolean value(PsiElement psiElement) {
        return psiElement instanceof RPsiElement && isLibraryElement(psiElement);
    }
}
