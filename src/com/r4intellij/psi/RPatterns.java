package com.r4intellij.psi;

import com.intellij.patterns.PatternCondition;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RMemberExpression;
import org.jetbrains.annotations.NotNull;

import static com.intellij.patterns.PlatformPatterns.psiElement;

/**
 * @author Holger Brandl
 */
public class RPatterns {

    public static final PsiElementPattern.Capture<PsiElement> MEMBER_ASSIGNMENT_PATTERN = psiElement()
            .withParent(psiElement(RAssignmentStatement.class).with(new PatternCondition<RAssignmentStatement>("isMemberAssignee") {
                @Override
                public boolean accepts(@NotNull RAssignmentStatement psiElement, ProcessingContext processingContext) {
                    return psiElement.getAssignee() instanceof RMemberExpression;
                }
            }));
}
