package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Query;
import com.r4intellij.RPsiUtils;
import com.r4intellij.psi.RElementFactory;
import com.r4intellij.psi.api.*;
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
        public void visitAssignmentStatement(@NotNull RAssignmentStatement element) {
            PsiElement assignee = element.getAssignee();

            if (assignee instanceof RReferenceExpression &&
                    RPsiUtils.isNamedArgument((RReferenceExpression) assignee)) {
                return;
            }

//            // don't warn about simple assingments which we tag as non-resolvable
//            if (assignee instanceof RReferenceExpression &&
//                    RPsiUtils.isVarDeclaration((RReferenceExpression) assignee)) {
//                return;
//            }

            //is last statement in function expression (which are) return values in R (see unit-tests)
            if (RPsiUtils.isReturnValue(element)) return;

            // handle special attribute setters and inplace array-place modifications (see unit-tests)
            if (isInplaceAssignment(assignee)) return;


            // because the reference refers to the assignment and not just the assignee, we search for assignment refs here

            // use function or loop bariier here
            Query<PsiReference> search = ReferencesSearch.search(element, new LocalSearchScope(element.getContainingFile()));
            PsiReference first = search.findFirst();


            if (first == null) {
                myProblemHolder.registerProblem(assignee,
                        "Variable '" + assignee.getText() + "' is never used",
                        ProblemHighlightType.LIKE_UNUSED_SYMBOL);
            }
        }


        private boolean isInplaceAssignment(PsiElement assignee) {
            if (assignee instanceof RSubscriptionExpression) return true;

            if (!(assignee instanceof RCallExpression)) return false;

            RCallExpression callExpression = (RCallExpression) assignee;

            // Check if we can resolve it into a accessor setter

            // See https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Attributes
            // See https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Function-calls

            String methodName = callExpression.getExpression().getReference().getCanonicalText();

            // see if we can resolve it into an accessor function
            PsiElement accesorResolvant = RElementFactory.createFuncallFromText(
                    assignee.getProject(), "`" + methodName + "<-`()"
            ).getExpression().getReference().resolve();

            return accesorResolvant != null;
        }


    }
}
