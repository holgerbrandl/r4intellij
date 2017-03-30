/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor;

import com.intellij.codeInsight.TargetElementEvaluatorEx;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author Dennis.Ushakov
 * @author holgerbrandl
 *         <p>
 *         See https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000126084-Incorrect-help-when-caret-at-word-end?page=1
 */
public class RTargetElementEvaluator implements TargetElementEvaluatorEx {
    @Override
    public boolean includeSelfInGotoImplementation(@NotNull PsiElement element) {
        return false;
    }


    @Nullable
    @Override
    public PsiElement getElementByReference(@NotNull PsiReference ref, int flags) {
//    if (ref instanceof JSTextReference) {
//      final PsiElement element = ref.getElement();
//      final JSCallExpression call = PsiTreeUtil.getParentOfType(element, JSCallExpression.class);
//      final JSExpression expression = call != null ? call.getMethodExpression() : null;
//      if (expression instanceof JSReferenceExpression) {
//        JSReferenceExpression callee = (JSReferenceExpression)expression;
//        JSExpression qualifier = callee.getQualifier();
//
//        if (qualifier != null && AngularJSIndexingHandler.INTERESTING_METHODS.contains(callee.getReferencedName()) &&
//            AngularIndexUtil.hasAngularJS(element.getProject())) {
//          return element;
//        }
//      }
//    }
        return ref.getElement().getReference().resolve();
//    return ref.resolve();
//    return null;
    }


    @Override
    public boolean isIdentifierPart(PsiFile psiFile, CharSequence charSequence, int i) {
        return Character.isJavaIdentifierPart(charSequence.charAt(i));
    }
}
