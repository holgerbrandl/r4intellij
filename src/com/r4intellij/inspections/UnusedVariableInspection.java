package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Query;
import com.r4intellij.RPsiUtils;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RReferenceExpression;
import com.r4intellij.psi.api.RVisitor;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

/**
 * Flag unused variables. We never flag functions calls (even when not being assigned) because
 * of potential side effects.
 */
public class UnusedVariableInspection extends RInspection {


    @Nls
    @NotNull
    @Override
    public String getDisplayName() {
        return "Unused Variable or Function";
    }


    @NotNull
    @Override
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
        return new Visitor(holder);
    }


    private class Visitor extends RVisitor { // todo don't we need to be recursive here

        private final ProblemsHolder myProblemHolder;


        public Visitor(@NotNull final ProblemsHolder holder) {
            myProblemHolder = holder;
        }


        @Override
        public void visitAssignmentStatement(@NotNull RAssignmentStatement o) {


            PsiElement assignee = o.getAssignee();

            if (RPsiUtils.isNamedArgument((RReferenceExpression) assignee)) {
                return;
            }

            //is last statement in function expression (which are) return values in R
            if (RPsiUtils.isReturnValue(o)) return;


            Query<PsiReference> search = ReferencesSearch.search(assignee);
            PsiReference first = search.findFirst();

            if (first == null) {
                myProblemHolder.registerProblem(assignee,
                        "Variable '" + assignee.getText() + "' is never used",
                        ProblemHighlightType.LIKE_UNUSED_SYMBOL);
            }
        }


    }
}
