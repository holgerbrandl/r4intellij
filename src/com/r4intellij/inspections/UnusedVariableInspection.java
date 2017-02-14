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
import com.r4intellij.psi.RElementFactory;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.psi.api.RCallExpression;
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

            //is last statement in function expression (which are) return values in R
            if (RPsiUtils.isReturnValue(element)) return;

            if (isAccessorSetterCall(assignee)) return;


            // because the reference refers to the assignment and not just the assignee, we search for assignment refs here
            Query<PsiReference> search = ReferencesSearch.search(element);
            PsiReference first = search.findFirst();
//            search.findAll()

//            PsiReference first;
//            Iterator<PsiReference> searchIt = search.iterator();
//
//            while (searchIt.hasNext()) {
//                PsiElement resolvant = searchIt.next().getElement();
//
//                boolean isSelfRef = (resolvant instanceof RAssignmentStatement) &&
//                        ((RAssignmentStatement) resolvant).getAssignee().equals(element);
//
//                if()
//                first = search.findFirst();
//            }

            if (first == null) {
                myProblemHolder.registerProblem(assignee,
                        "Variable '" + assignee.getText() + "' is never used",
                        ProblemHighlightType.LIKE_UNUSED_SYMBOL);
            }
        }


        private boolean isAccessorSetterCall(PsiElement assignee) {
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
